{-# LANGUAGE Arrows, OverloadedStrings, LambdaCase #-}

module Utils.Yukari.Parser (parsePage, parseYenPage) where


import           Control.Applicative
import           Control.Lens hiding (deep)
import qualified Data.Attoparsec.Text as A
import           Data.Char
import           Data.List
import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.Text (pack, unpack, split, Text)
import qualified Data.Text as T
import           Data.Tree.NTree.TypeDefs
import           Text.HandsomeSoup
import           Text.Read (readMaybe)
import           Text.XML.HXT.Core
import           Utils.Yukari.Settings
import           Utils.Yukari.Types

parseAnimeInfo :: String -> Information
parseAnimeInfo info =
  AnimeInformation AnimeInfo { _releaseFormat = parseFormat info
                             , _videoContainer = parseContainer info
                             , _animeCodec = parseCodec info
                             , _subtitles = parseSubs info
                             , _resolution = parseResolution info
                             , _audio = parseAudio info
                             }

parseCodec :: String -> Maybe AnimeCodec
parseCodec info =
  let i' = splitInfoS info
  in if length i' >= 3
     then let c = i' !! 2
          in case () of _ | "WMV" `isInfixOf` c -> Just WMV_
                          | "Hi10P" `isInfixOf` c -> Just H264HI10P
                          | "h264" `isInfixOf` c -> Just H264
                          | "ISO" `isInfixOf` info ->
                            readMaybe $ head i'
                          | otherwise -> readMaybe c
     else Nothing

parseMangaInfo :: String -> Information
parseMangaInfo info =
  MangaInformation MangaInfo { _scanlated = "Scanlated" `isInfixOf` info
                             , _archived = not $ "Unarchived" `isInfixOf` info
                             , _ongoing = "Ongoing" `isInfixOf` info
                             }

parseContainer :: String -> Maybe AnimeContainer
parseContainer info =
  let i' = splitInfoS info
  in if length i' >= 2
     then let c = head $ tail i'
          in case () of _ | "ISO" `isInfixOf` c -> Just ISO
                          | "VOB" `isInfixOf` c -> Just VOB
                          | "M2TS" `isInfixOf` c -> Just M2TS
                          | otherwise -> readMaybe c
     else Nothing

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
    dts s'@(_:xs)
      | "| DTS " `isPrefixOf` s' = Just . DTS . takeWhile (/= ' ') $
                                    drop (length ("| DTS " :: String)) s'
      | otherwise = dts xs

parseFormat :: String -> Maybe ReleaseFormat
parseFormat info = case splitInfoS info of
  [] -> Nothing
  x:_ | "DVD" `isInfixOf` x -> Just DVD
      | otherwise -> readMaybe x

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

safeApply :: ([t] -> [a]) -> [t] -> [a]
safeApply _ [] = []
safeApply f xs = f xs

anyInfix :: String -> [String] -> Bool
anyInfix x = any (`isInfixOf` x)

extractSubs :: String -> String
extractSubs x =
  let xs = filter (`anyInfix` ["Hardsubs", "Softsubs", "RAW"]) $ splitInfoS x
  in safeApply last xs

splitsize :: String -> (Double, String)
splitsize s = (read $ head as, head $ tail as)
    where as = map unpack $ split (== ' ') (pack s)

sizeToBytes :: String -> Integer
sizeToBytes size = round $ s * 1024 ^ getExp m
  where (s, m) = splitsize $ delete ',' size
        getExp :: String -> Int
        getExp n = fst . head . filter (\(_, u) -> n == u) $
                     zip [0, 1..] ["B", "KB", "MB" , "GB", "TB", "PB"]


stripeg :: Char -> String -> String
stripeg t = reverse . takeWhile (/= t) . reverse

stripeq :: String -> String
stripeq = stripeg '='

stripID :: String -> Integer
stripID = read . stripeq

parseResolution :: String -> Maybe Resolution
parseResolution = either (const Nothing) Just . A.parseOnly r . pack
  where
    r :: A.Parser Resolution
    r = pp <|> reg <|> (A.anyChar *> r)
    pp = "480p" *> return (Resolution 640 480)
         <|> "720p" *> return (Resolution 1280 720)
         <|> "1080p" *> return (Resolution 1920 1080)
    reg = Resolution <$> A.decimal <* "x" <*> A.decimal


splitInfoS :: String -> [String]
splitInfoS = map unpack . splitInfo . pack

splitInfo :: Text -> [Text]
splitInfo = filter (not . T.null) . map dropOuterSpaces . split (== '|')

dropOuterSpaces :: Text -> Text
dropOuterSpaces = T.dropWhile (== ' ') . T.dropWhileEnd (== ' ')

nameAttr
  :: ArrowXml cat => String
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
  nameAttr "tr" "class" isInfixOf "torrent  " >>>
    proc x -> do
      tID <- getAttrValue "id" -< x
      tInfSuf <- concat .< (getText
                            <\\ processTopDown (filterA . neg $ hasName "img")
                            <<< hasAttrValue "href" infixIds
                            <<< deep (hasName "a") <<< deep (hasName "td")) -< x
      tLink <- getAttrValue "href" <<< hasAttrValue "href" infixIds
               <<< deep (hasName "a") <<< deep (hasName "td") -< x
      tDown <- css "a" ! "href" <<< getCssAttr "a" "title" "Download" -< x
      tstch <- deep getText <<< getTorP "torrent_snatched" -< x
      tlchr <- deep getText <<< getTorP "torrent_leechers" -< x
      tsdr <- deep getText <<< getTorP "torrent_seeders" -< x
      tsize <- deep getText <<< getTorP "torrent_size" -< x
      returnA -< ABTorrent { _torrentID = read $ stripeg '_' tID
                           , _torrentURI = mainpage tLink
                           , _torrentDownloadURI = mainpage tDown
                           , _torrentInfoSuffix = tInfSuf
                           , _torrentInfo = Nothing
                           , _torrentSnatched = read tstch
                           , _torrentLeechers = read tlchr
                           , _torrentSeeders = read tsdr
                           , _torrentSize = sizeToBytes tsize
                           }
  where
    infixIds x = "php?id=" `isInfixOf` x && "torrentid=" `isInfixOf` x
    mainpage x = ys ^. siteSettings . baseSite ++ "/" ++ x

extractTorrentGroups
  :: ArrowXml cat =>
     cat a (NTree XNode) -> YukariSettings -> cat a ABTorrentGroup
extractTorrentGroups doc ys =
  doc //> css "div" >>> havp "class" "group_cont" >>>
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
    tors <- listA (getTorrent ys) <<< hasAttrValue "class" (== "torrent_group")
            <<< multi (hasName "table") -< x
    let cat' = parseCategory cat
    returnA -< ABTorrentGroup
                 { _torrentName = title
                 , _torrentCategory = cat'
                 , _seriesID = stripID serID
                 , _groupID = stripID grID
                 , _torrentImageURI = img
                 , _torrentTags = map stripeq tags
                 , _torrents = [attachInfo x' . parseInfo cat' $
                                _torrentInfoSuffix x' | x' <- tors]
                 }
  where
    havp atr content = hasAttrValue atr $ isInfixOf content
    havpa page = havp "href" (page ++ ".php") <<< css "a" <<< gTit
    attachInfo t i = t & torrentInfo .~ i

parseInfo :: Maybe Category -> String -> Maybe Information
parseInfo (Just Anime) suf = Just $ parseAnimeInfo suf
parseInfo (Just LiveAction) suf = Just $ parseAnimeInfo suf
parseInfo (Just LiveActionSeries) suf = Just $ parseAnimeInfo suf
parseInfo (Just (Manga _)) suf = Just $ parseMangaInfo suf
parseInfo _ _ = Nothing

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
      site = ys ^. siteSettings . baseSite
  n <- runX $ extractNextPage doc
  gs <- runX $ extractTorrentGroups doc ys
  let next = safeApply (\x -> site ++ "/" ++ head x) n
  return (next, map (ys ^. siteSettings . groupPreprocessor) gs)

parseYenPage :: String -> IO YenPage
parseYenPage body = do
  let doc = parseHtml body
  yen <- runX $ doc //> hasAttrValue "href" (== "/konbini.php") /> getText
  links <- runX $ (doc //> hasName "a" >>> getAttrValue "href")
           >>. filter isExchange
  return YenPage { yenOwned = yenToInt $ head yen
                 , spendingLinks = mapMaybe linksToCostLinks links
                 }
    where
      yenToInt = read . reverse . takeWhile isDigit . reverse . filter (/= ',')
      isExchange = isInfixOf "action=exchange"
      linksToCostLinks x
        | "trade=1" `isInfixOf` x = Just (1000, x)
        | "trade=2" `isInfixOf` x = Just (10000, x)
        | "trade=3" `isInfixOf` x = Just (100000, x)
        | "trade=4" `isInfixOf` x = Just (1000000, x)
        | otherwise = Nothing

parseCategory :: String -> Maybe Category
parseCategory cat
    | cat `elem` [ "Single", "Soundtrack"
                 , "DVD", "Live", "PV"
                 , "Live Album" , "Compilation", "Album"
                 , "Drama CD", "EP"] = Music <$> readMaybe cat
    | cat == "Remix CD" = Just $ Music RemixCD
    | cat `elem` [ "Manga", "Oneshot", "Manhua"
                 , "Manhwa", "OEL"] = if cat == "Manga"
                                      then Just $ Manga GenericManga
                                      else Manga <$> readMaybe cat
    | cat == "Visual Novel" = Just VisualNovel
    | cat == "Light Novel" = Just LightNovel
    | cat == "Live Action Movie" = Just LiveAction
    | cat == "Live Action TV Series" = Just LiveActionSeries
    | cat == "Game Guide" = Just GameGuide
    | otherwise = readMaybe cat
