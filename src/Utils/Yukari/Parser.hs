{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings #-}

module Utils.Yukari.Parser (parsePage, parseYenPage) where

import qualified Data.Attoparsec.Text as A
import           Data.Char
import           Data.List
import           Data.Text (pack, unpack, split)
import           System.Directory
import           System.FilePath
import           Text.HandsomeSoup
import           Text.XML.HXT.Core
import           Utils.Yukari.Formatter
import           Utils.Yukari.Settings
import           Utils.Yukari.Types

parseAnimeInfo :: String -> Information
parseAnimeInfo info = AnimeInformation AnimeInfo { releaseFormat = parseFormat info
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
parseMangaInfo info = MangaInformation MangaInfo { scanlated = "Scanlated" `isInfixOf` info
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
  | otherwise = OtherAudio s

parseFormat :: String -> ReleaseFormat
parseFormat info
  | "DVD" `isInfixOf` f = DVD
  | otherwise = read f
  where f = dropSpaces $ head $ splitInfo info

parseSubs :: String -> Subtitles
parseSubs info
  | "RAW" `isInfixOf` sub = RAW
  | "Softsubs" `isInfixOf` sub = Softsub $ extractParens sub
  | "Hardsubs" `isInfixOf` sub = Softsub $ extractParens sub
  where sub = extractSubs info
        extractParens x = if '(' `elem` x then init $ tail $ dropWhile (/= '(') x else ""

anyInfix :: String -> [String] -> Bool
anyInfix x l = True `elem` [b `isInfixOf` x | b <- l]

extractSubs :: String -> String
extractSubs x = last $ filter (`anyInfix` ["Hardsubs", "Softsubs", "RAW"]) (splitInfo x)

splitsize :: String -> (Double, String)
splitsize s = ((read $ head as) :: Double, head $ tail as)
    where as = map unpack $ split (== ' ') (pack s)

sizeToBytes :: String -> Integer
sizeToBytes size = round $ s * 1024 ^ getExp m
                    where (s, m) = splitsize $ delete ',' size
                          getExp m = fst $ head $ filter (\(e, u) -> m == u) (zip [0, 1..] ["B", "KB", "MB", "GB", "TB", "PB"])


stripeg :: Char -> String -> String
stripeg t = reverse . takeWhile (/= t) . reverse

stripeq :: String -> String
stripeq = stripeg '='

stripID :: String -> Integer
stripID = read . stripeq


parseResolution :: String -> Resolution
parseResolution s
  | "| 1080p |" `isInfixOf` s = Resolution 1920 1080
  | "| 720p |" `isInfixOf` s = Resolution 1280 720
  | otherwise = uncurry Resolution ext
  where extractRes x = (\t -> ((read $ filter isDigit $ head t) :: Integer, (read $ filter isDigit $ last t) :: Integer)) $ map unpack $ split (== 'x') $ pack $ head $ filter (isInfixOf "x") (splitInfo x)
        ext = extractRes s

splitInfo :: String -> [String]
splitInfo x = filter (not . null) $ map (dropSpaces . unpack) (split (== '|') (pack x))

dropSpaces :: String -> String
dropSpaces "" = ""
dropSpaces " " = ""
dropSpaces s = let x = if head s == ' ' then tail s else s in
               if last x == ' ' then init x else x

procSuff suff = if " | " `isPrefixOf` reverse suff then reverse $ drop 3 $ reverse suff else suff

nameAttr name attrV pred comp = deep (hasName name) >>> hasAttrValue attrV (pred comp)
nAt = nameAttr "span" "class" (==)
gTit = nAt "group_title"
text = getChildren >>> getText

(.<) = flip (>.)
(.<<) = flip (>>.)
(<\) = flip (/>)
(<\\) = flip (//>)


getCssAttr t attr eq = css t >>> hasAttrValue attr (== eq)
getTorP = getCssAttr "td" "class"

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
        returnA -< ABTorrent { torrentID = read $ stripeg '_' tID, torrentURI = mainpage ++ tLink, torrentDownloadURI = mainpage ++ tDown
                             , torrentInfoSuffix = procSuff tInfSuf, torrentInfo = NoInfo, torrentSnatched = read tstch
                             , torrentLeechers = read tlchr, torrentSeeders = read tsdr, torrentSize =  sizeToBytes tsize
                             }

--extractTorrentGroups :: String -> [ABTorrentGroup]
extractTorrentGroups doc ys = doc //> css "div" >>> havp "class" "group_cont" >>>
  proc x -> do
    cat <- text <<< nameAttr "span" "class" (==) "cat"  -< x
    img <- css "img" ! "src" <<< nAt "mainimg" -< x
    serID <- getAttrValue "href" <<< havp "href" "series.php"
             <<< css "a" <<< gTit-< x
    grID <- getAttrValue "href" <<< havp "href" "torrents.php"
            <<< css "a" <<< gTit -< x
    title <- getText <<< getChildren <<< havpa "series"
             <<< css "a" <<< gTit -< x
    tags <- listA $ css "a" ! "href" <<< hasAttrValue "class" (== "tags_sm")
            <<< multi (hasName "div") -< x
    tors <- listA (getTorrent ys) <<< hasAttrValue "class" (== "torrent_group") <<< multi (hasName "table") -< x
    returnA -< ABTorrentGroup { torrentName = title, torrentCategory = parseCategory cat, seriesID = stripID serID
                              , groupID = stripID grID, torrentImageURI = img, torrentTags = map stripeq tags
                              , torrents = map (\x -> attachInfo x $ parseInfo (parseCategory cat) (torrentInfoSuffix x)) tors
                              }
  where
    havp atr content = hasAttrValue atr $ isInfixOf content
    havpa page = havp "href" (page ++ ".php") <<< css "a" <<< gTit


parseInfo :: Category -> String -> Information
parseInfo Anime suf = parseAnimeInfo suf
parseInfo (Manga _) suf = parseMangaInfo suf
parseInfo cat suf = NoInfo

attachInfo :: ABTorrent -> Information -> ABTorrent
attachInfo t i = t {torrentInfo = i}

extractNextPage doc = doc /> css "div" >>> hasAttrValue "class" (== "pages")
                      >>> css "a" >>> proc elem -> do
  text <- deep getText -< elem
  if "Next" `isInfixOf` text
    then getAttrValue "href" -< elem
    else zeroArrow -< ()

parsePage :: YukariSettings -> String -> IO (String, [ABTorrentGroup])
parsePage ys html = do
  let doc = parseHtml html
      site = baseSite $ siteSettings ys
  n <- runX $ extractNextPage doc
  gs <- runX $ extractTorrentGroups doc ys
  let next = if null n then "" else site ++ "/" ++ head n
  return (next, gs)

parseYenPage :: String -> IO YenPage
parseYenPage body = do
  let doc = parseHtml body
  yen <- runX $ doc //> hasAttrValue "href" (== "/konbini.php") /> getText
  links <- runX $ (doc //> hasName "a" >>> getAttrValue "href") >>. filter isExchange
  return YenPage { yenOwned = yenToInt $ head yen
                 , spendingLinks = map linksToCostLinks links
                 }
    where yenToInt = read . reverse . takeWhile isDigit . reverse . filter (/= ',')
          isExchange = isInfixOf "action=exchange"
          linksToCostLinks x
            | "trade=1" `isInfixOf` x = (1000, x)
            | "trade=2" `isInfixOf` x = (10000, x)
            | "trade=3" `isInfixOf` x = (100000, x)
            | "trade=4" `isInfixOf` x = (1000000, x)

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
