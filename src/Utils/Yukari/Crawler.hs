module Utils.Yukari.Crawler (crawlFromURL, crawlFromFile, getSinglePage) where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Concurrent
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
import           Utils.Yukari.Parser (parsePage, parseYenPage)
import           Utils.Yukari.Settings
import           Utils.Yukari.Types

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

-- | We check if we're banned first before we even try to log in.
-- While it is an extra GET, we don't POST account information needlessly
-- and this is only done once per log in anyway.
banned :: SiteSettings -> IO Bool
banned s = do
  (_, body) <- curlGetString (loginSite s) []
  return $ "You are banned from logging in for another " `isInfixOf` body

-- | As someone thought that _obviously_ the best way to inform
-- the user about a failed login would be a JavaScript pop-up, we have to hack
-- around the braindead defaults.
--
-- We try to fetch the index page and if we can see the forums.php in the body,
-- we probably made it.
loggedIn :: SiteSettings -> Curl -> IO Bool
loggedIn s c = do
  body <- getInternalPage c (baseSite s)
  return $ "forums.php" `isInfixOf` body


logonCurl :: SiteSettings -> IO Curl
logonCurl s = do
  b <- banned s
  when b $ do
    putStrLn "You have been banned from logging in. Check the site."
    exitFailure

  let fields = CurlPostFields [ "username=" ++ username s, "password=" ++ password s ] : method_POST
  curl <- initialize
  setopts curl [ CurlCookieJar "cookies", CurlUserAgent defaultUserAgent, CurlTimeout 15 ]
  r <- do_curl_ curl (loginSite s) fields :: IO CurlResponse
  l <- loggedIn s curl
  if respCurlCode r /= CurlOK || not l
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
buildTorrentPaths set group = map (makePath &&& torrentDownloadURI) $ torrents group
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
