{-# LANGUAGE LambdaCase #-}
module Utils.Yukari.Crawler (crawlFromURL, crawlFromFile, getSinglePage) where

import Control.Lens hiding ((<.>))
import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.List
import           Network.Curl
import           Network.Curl.Download
import           Network.HTTP hiding (password)
import           System.Directory
import           System.Exit
import           System.FilePath
import           Utils.Yukari.Formatter
import           Utils.Yukari.Parser (parsePage)
import           Utils.Yukari.Settings
import           Utils.Yukari.Types

type URL = String

-- | Filter out unwanated torrents as per specified function.
torrentFilter :: (ABTorrent -> Bool) -> [ABTorrentGroup] -> [ABTorrentGroup]
torrentFilter p = filter (null . view torrents) . map (torrents %~ filter p)

-- | Use the curl session to fetch a possibly login restricted page.
getInternalPage :: YukariSettings -> Curl -> String -> IO (Maybe String)
getInternalPage ys curl url = do
  r <- do_curl_ curl url method_GET :: IO CurlResponse
  if respCurlCode r /= CurlOK
    then retryFetch r $ _connectionRetries ys
    else return . Just $ respBody r
  where
    retryFetch :: CurlResponse -> Integer -> IO (Maybe String)
    retryFetch r ret
      | ret <= 0 = do
          verbPrint Low ys [ "Failed to fetch:", url, ";"
                           , show $ respCurlCode r, "--"
                           , respStatusLine r
                           ]
          return Nothing
      | otherwise = do
          verbPrint High ys [ "Failed to fetch", url
                            , show $ ret - 1
                            , attemptFormat $ ret - 1
                            , "remaining."
                            ]
          getInternalPage (ys { _connectionRetries = ret - 1 }) curl url
      where
        attemptFormat 1 = "attempt"
        attemptFormat _ = "attempts"

-- | We check if we're banned first before we even try to log in.
-- While it is an extra GET, we don't POST account information needlessly
-- and this is only done once per log in anyway.
banned :: SiteSettings -> IO Bool
banned s = do
  (_, body) <- curlGetString (s ^. loginSite) []
  return $ "You are banned from logging in for another " `isInfixOf` body

-- | As someone thought that _obviously_ the best way to inform
-- the user about a failed login would be a JavaScript pop-up, we have to hack
-- around the braindead defaults.
--
-- We try to fetch the index page and if we can see the forums.php in the body,
-- we probably made it.
loggedIn :: YukariSettings -> Curl -> IO Bool
loggedIn s c = do
  let settings = s ^. siteSettings
  body <- getInternalPage s c (settings ^. baseSite)
  case body of
    Nothing -> error "Failed to successfully grab the login page."
    Just body' -> do
      let b = "forums.php" `isInfixOf` body'
      unless b $ verbPrint Debug s [ "Failed to log in. username:"
                                   , settings ^. username
                                   , "password:"
                                   , settings ^. password
                                     ++ "\nBody:\n" ++ body'
                                   ]
      return $ "forums.php" `isInfixOf` body'

-- | Log in to the site.
logonCurl :: YukariSettings -> IO Curl
logonCurl ys = do
  let s = ys ^. siteSettings
  b <- banned s
  when b $ do
    putStrLn "Seems you have been banned from logging in. Check the site."
    exitFailure

  let fields = CurlPostFields [ "username=" ++ s ^. username
                              , "password=" ++ s ^. password ] : method_POST
  curl <- initialize
  setopts curl [ CurlCookieJar "cookies", CurlUserAgent defaultUserAgent
               , CurlTimeout 15 ]
  r <- do_curl_ curl (s ^. loginSite) fields :: IO CurlResponse
  l <- loggedIn ys curl
  if respCurlCode r /= CurlOK || not l
    then error $ concat ["Failed to log in as ", s ^. username, ": "
                        , show $ respCurlCode r, " -- ", respStatusLine r]
    else return curl

-- | Crawl the page according to user settings and using a specified curl
-- session. We can obtain the appropiate session using 'logonCurl'.
crawl :: YukariSettings -> Curl -> String -> IO ()
crawl _ _ "" = return ()
crawl ys curl url
  | ys ^. maxPages <= 0 = return ()
  | otherwise = do
    let settings = ys ^. siteSettings
    verbPrint Low ys ["Crawling", url]
    body <- getInternalPage ys curl url
    case body of
      Nothing -> error $ "Failed to crawl " ++ url
      Just body' -> do
        verbPrint Debug ys ['\n' : body']
        pa@(nextPage, groups) <- parsePage ys body'
        when (ys ^. logVerbosity >= High) (prettyPage pa)
        verbPrint Low ys ["Have", show . sum $ map (length . _torrents) groups
                         , "torrents pre-filter."]
        let filtered' = torrentFilter (_filterFunc settings) groups
        let tPaths = concatMap (buildTorrentPaths settings) filtered'
        verbPrint Low ys ["Have", show $ length tPaths, "torrents post-filter."]
        mapM_ (\(fp, url') -> download fp url' ys) tPaths
        crawl (ys { _maxPages = _maxPages ys - 1 }) curl nextPage

-- | We take settings for the site and a torrent group listing and we try to
-- assign a file path to each torrent in the group to which the download
-- will be made.
buildTorrentPaths :: SiteSettings -> ABTorrentGroup -> [(Maybe FilePath, URL)]
buildTorrentPaths sett g =
  map (makePath &&& _torrentDownloadURI) $ _torrents g
  where
    makePath :: ABTorrent -> Maybe FilePath
    makePath tor = case _torrentCategory g of
      Nothing -> Nothing
      Just cat -> foldl (liftA2 (</>)) (_topWatch sett)
                  [ _watchFunc sett cat
                  , Just $ unwords [ _torrentName g, "-"
                                   , show cat
                                   , "~"
                                   , _torrentInfoSuffix tor <.> "torrent"
                                   ]
                  ]

-- | Starts the crawl from a saved page.
crawlFromFile :: YukariSettings -> FilePath -> IO ()
crawlFromFile ys f = do
  curl <- logonCurl ys
  body' <- readFile f
  (n, _) <- parsePage ys body'
  crawl ys curl n

-- | Starts the crawl from the URL specified in the settings.
crawlFromURL :: YukariSettings -> IO ()
crawlFromURL ys = do
  let settings = ys ^. siteSettings
  curl <- logonCurl ys
  crawl ys curl $ _searchSite settings

-- | Logs the user in with 'logonCurl' and fetches a single page using
-- 'getInternalPage'. Useful when we just want to grab something quickly.
getSinglePage :: YukariSettings -> String -> IO (Maybe String)
getSinglePage ys url = logonCurl ys >>= flip (getInternalPage ys) url

-- | Downloads a file to the specified location. If the file path is Nothing,
-- the download isn't performed.
download :: Maybe FilePath -- ^ The file to save to.
            -> URL -- ^ The URL to download from.
            -> YukariSettings -> IO ()
download path url ys =
  let clobber = ys ^. siteSettings . clobberFiles
      dry = DryRun `elem` ys ^. programSettings
  in
  case path of
    Nothing -> verbPrint Low ys ["Skipping empty path."]
    Just p -> do
      b <- doesFileExist p
      when b $ verbPrint Low ys ["Skipping already downloaded", p]
      if dry
        then verbPrint Debug ys [ "Dry run enabled, would download", url
                                , "to", p, "otherwise."]
        else when (clobber || not b) $ openURI url >>= \case
          Left e -> putStrLn $ unwords [ "Error ", e
                                       , " occured when downloading from "
                                       , url]
          Right bs -> verbPrint Low ys ["Downloading ", p]
                      >> createDirectoryIfMissing True (takeDirectory p)
                      >> BS.writeFile p bs
