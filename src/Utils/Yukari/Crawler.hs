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

-- | 'uncurry' for triples.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- | Replace all occurances of an element in a list with another.
replace :: Eq a => a -> a -> [a] -> [a]
replace c i [] = []
replace c i (x:xs) = if x == c then i : replace c i xs else x : replace c i xs

-- | Filter out unwanated torrents as per specified function.
torrentFilter :: (ABTorrent -> Bool) -> [ABTorrentGroup] -> [ABTorrentGroup]
torrentFilter _ [] = []
torrentFilter p (g:gs) = let ng = g { torrents = filter p $ torrents g } in
                         if 0 /= length (torrents ng) then ng : torrentFilter p gs else torrentFilter p gs

-- | Use the curl session to fetch a possibly login restricted page.
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
loggedIn :: YukariSettings -> Curl -> IO Bool
loggedIn s c = do
  let settings = siteSettings s
  body <- getInternalPage c (baseSite settings)
  let b = "forums.php" `isInfixOf` body
  unless b $ verbPrint Debug s [ "Failed to log in. username:"
                               , username settings
                               , "password:"
                               , password settings
                                 ++ "\nBody:\n" ++ body
                               ]
  return $ "forums.php" `isInfixOf` body

-- | Log in to the site.
logonCurl :: YukariSettings -> IO Curl
logonCurl ys = do
  let s = siteSettings ys
  b <- banned s
  when b $ do
    putStrLn "Seems you have been banned from logging in. Check the site."
    exitFailure

  let fields = CurlPostFields [ "username=" ++ username s
                              , "password=" ++ password s ] : method_POST
  curl <- initialize
  setopts curl [ CurlCookieJar "cookies", CurlUserAgent defaultUserAgent
               , CurlTimeout 15 ]
  r <- do_curl_ curl (loginSite s) fields :: IO CurlResponse
  l <- loggedIn ys curl
  if respCurlCode r /= CurlOK || not l
    then error $ concat ["Failed to log in as ", username s, ": "
                        , show $ respCurlCode r, " -- ", respStatusLine r]
    else return curl

-- | Crawl the page according to user settings and using a specified curl
-- session. We can obtain the appropiate session using 'logonCurl'.
crawl :: YukariSettings -> Curl -> String -> IO ()
crawl _ _ "" = return ()
crawl ys curl url = do
  let settings = siteSettings ys
  verbPrint Low ys ["Crawling", url]
  body <- getInternalPage curl url

  pa@(nextPage, groups) <- parsePage body
  when (logVerbosity ys >= High) (prettyPage pa)
  verbPrint Low ys ["Have", show . sum $ map (length . torrents) groups
                   , "torrents pre-filter."]
  let filtered = torrentFilter (filterFunc settings) groups
  let all = concatMap (buildTorrentPaths settings) filtered
  verbPrint Low ys ["Have", show $ length all, "torrents post-filter."]
  mapM_ (\(fp, url) -> download fp url ys) all
  crawl ys curl nextPage

-- | We take settings for the site and a torrent group listing and we try to
-- assign a file path to each torrent in the group to which the download
-- will be made.
buildTorrentPaths :: SiteSettings -> ABTorrentGroup -> [(Maybe FilePath, URL)]
buildTorrentPaths set group =
  map (makePath &&& torrentDownloadURI) $ torrents group
  where
    makePath :: ABTorrent -> Maybe FilePath
    makePath tor =
      foldl (liftA2 (</>)) (topWatch set)
      [ watchFunc set $ torrentCategory group
      , Just $ unwords [ torrentName group, "-"
                       , show $ torrentCategory group, "~"
                       , torrentInfoSuffix tor <.> "torrent"
                       ]
      ]

-- | Starts the crawl from a saved page.
crawlFromFile :: YukariSettings -> FilePath -> IO ()
crawlFromFile ys f = do
  let settings = siteSettings ys
  curl <- logonCurl ys
  body <- readFile f
  (n, _) <- parsePage body
  crawl ys curl n

-- | Starts the crawl from the URL specified in the settings.
crawlFromURL :: YukariSettings -> IO ()
crawlFromURL ys = do
  let settings = siteSettings ys
  curl <- logonCurl ys
  crawl ys curl $ searchSite settings

-- | Logs the user in with 'logonCurl' and fetches a single page using
-- 'getInternalPage'. Useful when we just want to grab something quickly.
getSinglePage :: YukariSettings -> String -> IO String
getSinglePage ys url = logonCurl ys >>= flip getInternalPage url

-- | Downloads a file to the specified location. If the file path is Nothing,
-- the download isn't performed.
download :: Maybe FilePath -- ^ The file to save to.
            -> URL -- ^ The URL to download from.
            -> YukariSettings -> IO ()
download path url ys =
  let clobber = clobberFiles $ siteSettings ys
      dry = DryRun `elem` programSettings ys
  in
  case path of
    Nothing -> verbPrint Low ys ["Skipping empty path."]
    Just p -> do
      b <- doesFileExist p
      when b $ verbPrint Low ys ["Skipping already downloaded", p]
      if dry
        then putStrLn $ unwords [ "Dry run enabled, would download ", url
                                , " to ", p, " otherwise."]
        else when (clobber || not b) $ do
          res <- openURI url
          case res of
            Left e -> putStrLn $
                      unwords [ "Error ", e, " occured when downloading from "
                              , url]
            Right bs -> verbPrint Low ys ["Downloading ", p]
                        >> createDirectoryIfMissing True (takeDirectory p)
                        >> BS.writeFile p bs
