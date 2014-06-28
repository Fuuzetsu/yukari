{-# LANGUAGE Arrows, OverloadedStrings, LambdaCase #-}

module Utils.Yukari.Parser (parsePage, parseYenPage) where

import Control.Applicative
import qualified Data.Attoparsec.Text as A
import           Data.Char
import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.List
import           Data.Text (pack, unpack, split)
import           Data.Tree.NTree.TypeDefs
import           Text.HandsomeSoup
import           Text.XML.HXT.Core
import           Utils.Yukari.Settings
import           Utils.Yukari.Types

parseAnimeInfo :: String -> Information
parseAnimeInfo info =
  AnimeInformation AnimeInfo { releaseFormat = parseFormat info
                             , videoContainer = parseContainer info
                             , animeCodec = parseCodec info
                             , subtitles = parseSubs info
                             , resolution = parseResolution info
                             , audio = parseAudio info
                             }

parseCodec :: String -> AnimeCodec
parseCodec info
  | "WMV" `isInfixOf` c = WMV_
  | "Hi10P" `isInfixOf` c = H264HI10P
  | "h264" `isInfixOf` c = H264
  | "ISO" `isInfixOf` info = read $ dropSpaces $ head $ splitInfo info
  | otherwise = read c
  where c = dropSpaces $ splitInfo info !! 2

parseMangaInfo :: String -> Information
parseMangaInfo info =
  MangaInformation MangaInfo { scanlated = "Scanlated" `isInfixOf` info
                             , archived = not $ "Unarchived" `isInfixOf` info
                             , ongoing = "Ongoing" `isInfixOf` info
                             }

parseContainer :: String -> AnimeContainer
parseContainer info
  | "ISO" `isInfixOf` c = ISO
  | "VOB" `isInfixOf` c = VOB
  | otherwise = read c
  where c = dropSpaces $ head $ tail $ splitInfo info

parseAudio :: String -> Audio
parseAudio s
  | "| MP3" `isInfixOf` s = MP3
  | "| FLAC" `isInfixOf` s = FLAC
  | "| AAC" `isInfixOf` s = AAC
  | "| AC" `isInfixOf` s = AC
  | "| PCM" `isInfixOf` s = MP3
  | otherwise = fromMaybe (OtherAudio s) $ dts s
  where
    dts :: String -> Maybe Audio
    dts [] = Nothing
    dts s@(_:xs)
      | "| DTS " `isPrefixOf` s = Just . DTS . takeWhile (/= ' ') $
                                    drop (length ("| DTS " :: String)) s
      | otherwise = dts xs

parseFormat :: String -> ReleaseFormat
parseFormat info
  | "DVD" `isInfixOf` f = DVD
  | otherwise = read f
  where f = dropSpaces $ head $ splitInfo info

parseSubs :: String -> Subtitles
parseSubs info
  | "RAW" `isInfixOf` sub = RAW
  | "Softsubs" `isInfixOf` sub = Softsub $ extractParens sub
  | "Hardsubs" `isInfixOf` sub = Hardsub $ extractParens sub
  | otherwise = UnknownSubs
  where sub = extractSubs info
        extractParens x = if '(' `elem` x
                          then init $ tail $ dropWhile (/= '(') x
                          else ""

anyInfix :: String -> [String] -> Bool
anyInfix x = any (`isInfixOf` x)

extractSubs :: String -> String
extractSubs x =
  last $ filter (`anyInfix` ["Hardsubs", "Softsubs", "RAW"]) (splitInfo x)

splitsize :: String -> (Double, String)
splitsize s = (read $ head as, head $ tail as)
    where as = map unpack $ split (== ' ') (pack s)

sizeToBytes :: String -> Integer
sizeToBytes size = round $ s * 1024 ^ getExp m
  where (s, m) = splitsize $ delete ',' size
        getExp n = fst . head . filter (\(_, u) -> n == u) $
                     zip [0, 1..] ["B", "KB", "MB" , "GB", "TB", "PB"]


stripeg :: Char -> String -> String
stripeg t = reverse . takeWhile (/= t) . reverse

stripeq :: String -> String
stripeq = stripeg '='

stripID :: String -> Integer
stripID = read . stripeq

parseResolution :: String -> Resolution
parseResolution s = case A.parseOnly r $ pack s of
  Left _ -> Resolution 0 0
  Right r -> r
  where
    r :: A.Parser Resolution
    r = pp <|> reg <|> (A.anyChar *> r)
    pp = "480p" *> return (Resolution 640 480)
         <|> "720p" *> return (Resolution 1280 720)
         <|> "1080p" *> return (Resolution 1920 1080)
    reg = Resolution <$> A.decimal <* "x" <*> A.decimal

splitInfo :: String -> [String]
splitInfo x = filter (not . null) $ map (dropSpaces . unpack) (split (== '|') (pack x))

dropSpaces :: String -> String
dropSpaces = concat . words

procSuff :: String -> String
procSuff suff = if " | " `isPrefixOf` reverse suff then reverse $ drop 3 $ reverse suff else suff

nameAttr
  :: ArrowXml cat =>
     String
     -> String
     -> (t -> String -> Bool)
     -> t
     -> cat (NTree XNode) XmlTree
nameAttr name attrV p comp = deep (hasName name) >>> hasAttrValue attrV (p comp)

nAt :: ArrowXml cat => String -> cat (NTree XNode) XmlTree
nAt = nameAttr "span" "class" (==)

gTit :: ArrowXml cat => cat (NTree XNode) XmlTree
gTit = nAt "group_title"

text :: ArrowXml cat => cat (NTree XNode) String
text = getChildren >>> getText

(.<) :: ArrowList a => ([c] -> d) -> a b c -> a b d
(.<) = flip (>.)

(<\\) :: (ArrowTree a, Tree t) => a (t c) d -> a b (t c) -> a b d
(<\\) = flip (//>)


getCssAttr
  :: ArrowXml cat =>
     String -> String -> String -> cat (NTree XNode) XmlTree
getCssAttr t a eq = css t >>> hasAttrValue a (== eq)

getTorP :: ArrowXml cat => String -> cat (NTree XNode) XmlTree
getTorP = getCssAttr "td" "class"

getTorrent
  :: ArrowXml cat => YukariSettings -> cat (NTree XNode) ABTorrent
getTorrent ys =
  let mainpage = baseSite $ siteSettings ys
  in nameAttr "tr" "class" isInfixOf "torrent  " >>>
      proc x -> do
        tID <- getAttrValue "id" -< x
        tInfSuf <- concat .< (getText <\\ processTopDown (filterA $ neg (hasName "img"))
                   <<< hasAttrValue "href" (\y -> ("php?id=" `isInfixOf` y) && ("torrentid=" `isInfixOf` y))
                   <<< deep (hasName "a") <<< deep (hasName "td")) -< x
        tLink <- getAttrValue "href" <<< hasAttrValue "href" (\y -> ("php?id=" `isInfixOf` y) && ("torrentid=" `isInfixOf` y))
                 <<< deep (hasName "a") <<< deep (hasName "td") -< x
        tDown <- css "a" ! "href" <<< getCssAttr "a" "title" "Download" -< x
        tstch <- deep getText <<< getTorP "torrent_snatched" -< x
        tlchr <- deep getText <<< getTorP "torrent_leechers" -< x
        tsdr <- deep getText <<< getTorP "torrent_seeders" -< x
        tsize <- deep getText <<< getTorP "torrent_size" -< x
        returnA -< ABTorrent { torrentID = read $ stripeg '_' tID
                             , torrentURI = mainpage ++ "/" ++ tLink
                             , torrentDownloadURI = mainpage ++ "/" ++ tDown
                             , torrentInfoSuffix = procSuff tInfSuf
                             , torrentInfo = NoInfo, torrentSnatched = read tstch
                             , torrentLeechers = read tlchr
                             , torrentSeeders = read tsdr
                             , torrentSize =  sizeToBytes tsize
                             }


extractTorrentGroups
  :: ArrowXml cat =>
     cat a (NTree XNode) -> YukariSettings -> cat a ABTorrentGroup
extractTorrentGroups doc ys = doc //> css "div" >>> havp "class" "group_cont" >>>
  proc x -> do
    cat <- text <<< css "a" <<< nameAttr "span" "class" (==) "cat"  -< x
    img <- css "img" ! "src" <<< nAt "mainimg" -< x
    serID <- getAttrValue "href" <<< havp "href" "series.php"
             <<< css "a" <<< gTit -< x
    grID <- getAttrValue "href" <<< havp "href" "torrents.php"
            <<< css "a" <<< gTit -< x
    title <- getText <<< getChildren <<< havpa "series"-< x
    tags <- listA $ css "a" ! "href" <<< hasAttrValue "class" (== "tags_sm")
            <<< multi (hasName "div") -< x
    tors <- listA (getTorrent ys) <<< hasAttrValue "class" (== "torrent_group") <<< multi (hasName "table") -< x
    returnA -< ABTorrentGroup { torrentName = title
                              , torrentCategory = parseCategory cat
                              , seriesID = stripID serID
                              , groupID = stripID grID
                              , torrentImageURI = img
                              , torrentTags = map stripeq tags
                              , torrents = map (\x' -> attachInfo x' $ parseInfo (parseCategory cat) (torrentInfoSuffix x')) tors
                              }
  where
    havp atr content = hasAttrValue atr $ isInfixOf content
    havpa page = havp "href" (page ++ ".php") <<< css "a" <<< gTit


parseInfo :: Category -> String -> Information
parseInfo Anime suf = parseAnimeInfo suf
parseInfo LiveAction suf = parseAnimeInfo suf
parseInfo LiveActionSeries suf = parseAnimeInfo suf
parseInfo (Manga _) suf = parseMangaInfo suf
parseInfo _ _ = NoInfo

attachInfo :: ABTorrent -> Information -> ABTorrent
attachInfo t i = t {torrentInfo = i}

extractNextPage
  :: (ArrowXml cat, ArrowChoice cat) =>
     cat a (NTree XNode) -> cat a String
extractNextPage doc = doc /> css "div" >>> hasAttrValue "class" (== "pages")
                      >>> css "a" >>> proc e -> do
  t <- deep getText -< e
  if "Next" `isInfixOf` t
    then getAttrValue "href" -< e
    else zeroArrow -< ()

parsePage :: YukariSettings -> String -> IO (String, [ABTorrentGroup])
parsePage ys html = do
  let doc = parseHtml html
      site = baseSite $ siteSettings ys
  n <- runX $ extractNextPage doc
  gs <- runX $ extractTorrentGroups doc ys
  let next = if null n then "" else site ++ "/" ++ head n
  return (next, map (groupPreprocessor $ siteSettings ys) gs)

parseYenPage :: String -> IO YenPage
parseYenPage body = do
  let doc = parseHtml body
  yen <- runX $ doc //> hasAttrValue "href" (== "/konbini.php") /> getText
  links <- runX $ (doc //> hasName "a" >>> getAttrValue "href") >>. filter isExchange
  return YenPage { yenOwned = yenToInt $ head yen
                 , spendingLinks = mapMaybe linksToCostLinks links
                 }
    where yenToInt = read . reverse . takeWhile isDigit . reverse . filter (/= ',')
          isExchange = isInfixOf "action=exchange"
          linksToCostLinks x
            | "trade=1" `isInfixOf` x = Just (1000, x)
            | "trade=2" `isInfixOf` x = Just (10000, x)
            | "trade=3" `isInfixOf` x = Just (100000, x)
            | "trade=4" `isInfixOf` x = Just (1000000, x)
            | otherwise = Nothing

parseCategory :: String -> Category
parseCategory cat
    | cat `elem` ["Single", "Soundtrack", "DVD", "Live", "PV", "Live Album"
                 , "Compilation", "Album", "Drama CD", "EP"] = Music (read cat :: MusicCategory)
    | cat == "Remix CD" = Music RemixCD
    | cat `elem` ["Manga", "Oneshot", "Manhua", "Manhwa", "OEL"] = Manga (if cat == "Manga" then GenericManga else read cat :: MangaCategory)
    | cat == "Visual Novel" = VisualNovel
    | cat == "Light Novel" = LightNovel
    | cat == "Live Action Movie" = LiveAction
    | cat == "Live Action TV Series" = LiveActionSeries
    | cat == "Game Guide" = GameGuide
    | otherwise = read cat
