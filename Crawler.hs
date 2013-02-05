module Crawler (crawlFromURL, crawlFromFile, getSinglePage) where

import Network.Curl
import Network.Curl.Download
import Network.HTTP hiding (password)
import System.FilePath
import System.Directory
import Control.Monad
import Control.Applicative
import qualified Data.ByteString as BS

import Parser
import Types
import Settings
import Formatter
import Control.Concurrent


type URL = String

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

replace :: (Eq a) => a -> a -> [a] -> [a]
replace c i [] = []
replace c i (x:xs) = if x == c then i : replace c i xs else x : replace c i xs


torrentFilter :: (ABTorrent -> Bool) -> [ABTorrentGroup] -> [ABTorrentGroup]
torrentFilter _ [] = []
torrentFilter p (g:gs) = let ng = g { torrents = filter p $ torrents g } in
                         if 0 /= length (torrents ng) then ng : torrentFilter p gs else torrentFilter p gs

getInternalPage :: Curl -> String -> IO String
getInternalPage curl url = do
  r <- do_curl_ curl url method_GET :: IO CurlResponse
  if respCurlCode r /= CurlOK
    then error $ "Failed to fetch: " ++ url ++ " ; " ++ show (respCurlCode r) ++ " -- " ++ respStatusLine r
    else return $ respBody r

logonCurl :: SiteSettings -> IO Curl
logonCurl s = do
  let fields = CurlPostFields [ "username=" ++ username s, "password=" ++ password s ] : method_POST
  curl <- initialize
  setopts curl [ CurlCookieJar "cookies", CurlUserAgent defaultUserAgent, CurlTimeout 15 ]
  r <- do_curl_ curl (loginSite s) fields :: IO CurlResponse
  if respCurlCode r /= CurlOK
    then error $ "Failed to log in as " ++ username s ++ ": " ++ show (respCurlCode r) ++ " -- " ++ respStatusLine r
    else return curl

crawl :: SiteSettings -> Curl -> String -> IO ()
crawl _ _ "" = return ()
crawl settings curl url = do
  when (logVerbosity settings >= Low) (putStrLn $ "Crawling " ++ url)
  body <- getInternalPage curl url
  pa@(nextPage, groups) <- parsePage body
  when (logVerbosity settings >= High) (prettyPage pa)
  when (logVerbosity settings >= Low) (putStrLn $ "Have " ++ show (sum (map (length . torrents) groups)) ++ " torrents pre-filter.")
  let filtered = torrentFilter (filterFunc settings) groups
  let all = concatMap (buildTorrentPaths settings) filtered
  when (logVerbosity settings >= Low) (putStrLn $ "Have " ++ show (length all) ++ " torrents post-filter.")
  mapM_ (\(fp, url) -> download fp url (logVerbosity settings) (clobberFiles settings)) all
  crawl settings curl nextPage

buildTorrentPaths :: SiteSettings -> ABTorrentGroup -> [(Maybe FilePath, URL)]
buildTorrentPaths set group = map (makePath Control.Arrow.&&& torrentDownloadURI) $ torrents group
  where makePath :: ABTorrent -> Maybe FilePath
        makePath tor = foldl (liftA2 (</>)) (topWatch set) [ watchFunc set $ torrentCategory group
                                                           , Just (torrentName group ++ " - " ++
                                                                   show (torrentCategory group) ++ " ~ " ++
                                                                   torrentInfoSuffix tor <.> "torrent")
                                                           ]

crawlFromFile :: SiteSettings -> FilePath -> IO ()
crawlFromFile settings f = do
  curl <- logonCurl settings
  body <- readFile f
  (n, _) <- parsePage body
  crawl settings curl n

crawlFromURL :: SiteSettings -> IO ()
crawlFromURL settings = do
  curl <- logonCurl settings
  crawl settings curl $ searchSite settings

getSinglePage :: SiteSettings -> String -> IO String
getSinglePage settings url = logonCurl settings >>= flip getInternalPage url


download :: Maybe FilePath -> String -> Verbosity -> Bool -> IO ()
download path url verb clobber = 
  case path of
    Nothing -> when (verb >= Low) (putStrLn "Skipping empty path.")
    Just p -> do
      b <- doesFileExist p
      when (verb >= Low && b) (putStrLn $ "Skipping already downloaded " ++ p)
      when (clobber || not b) $ do
        res <- openURI url
        case res of
          Left e -> putStrLn $ "Error " ++ e ++ " occured when downloading from " ++ url
          Right bs -> when (verb >= Low) (putStrLn ("Downloading " ++ p)) >>
                      createDirectoryIfMissing True (takeDirectory p) >>
                      BS.writeFile p bs
