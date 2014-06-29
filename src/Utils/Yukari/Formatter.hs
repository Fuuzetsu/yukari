module Utils.Yukari.Formatter where

import Control.Monad (when)
import Utils.Yukari.Types
import Utils.Yukari.Settings

verbPrint :: Verbosity -> YukariSettings -> [String] -> IO ()
verbPrint v ys s = when (v <= _logVerbosity ys) (putStrLn $ unwords s)

prettyGroup :: ABTorrentGroup -> String
prettyGroup g = _torrentName g ++ " - " ++ show (_torrentCategory g)
                ++ "\nImage: " ++ _torrentImageURI g
                ++ "\nTags: " ++ unwords (map (++ " ") $ _torrentTags g)
                ++ "\n" ++ unlines (map (indent.prettyTorrent) $ _torrents g)
  where indent l = unlines $ map ("  \t" ++ ) $ lines l

prettyTorrent :: ABTorrent -> String
prettyTorrent t = _torrentInfoSuffix t
                  ++ "\tSize: " ++ show (_torrentSize t)
                  ++ "\tSeeders: " ++ show (_torrentSeeders t)
                  ++ "\nLink: " ++ _torrentURI t
                  ++ "\nDownload: " ++ _torrentDownloadURI t
                  ++ "\n" ++ showM (fmap prettyInfo (_torrentInfo t))

prettyResolution :: Maybe Resolution -> String
prettyResolution Nothing = "Unknown"
prettyResolution (Just (Resolution w h)) = show w ++ " x " ++ show h

showM :: Show a => Maybe a -> String
showM Nothing = "Unknown"
showM (Just a) = show a

prettyInfo :: Information -> String
prettyInfo (AnimeInformation ai) =
  unlines [ "Format: " ++ showM (_releaseFormat ai)
          , "Container: " ++ showM (_videoContainer ai)
          , "Subtitles: " ++ prettySub (_subtitles ai)
          , "Resolution: " ++ prettyResolution (_resolution ai)
          , "Audio: " ++ show (_audio ai)
          ]
prettyInfo (MangaInformation mi) =
  unlines [ "Scanlated: " ++ boolYesNo (_scanlated mi)
          , "Archived: " ++ boolYesNo (_archived mi)
          , "Ongoing: " ++ boolYesNo (_ongoing mi)
          ]
  where
    boolYesNo b = if b then "Yes" else "No"

prettySub :: Subtitles -> String
prettySub (Softsub x) = "Softsubbed by " ++ x
prettySub (Hardsub x) = "Hardsubbed by " ++ x
prettySub RAW = "None"
prettySub UnknownSubs = "Unknown"

prettyPage :: (String, [ABTorrentGroup]) -> IO ()
prettyPage = mapM_ (putStrLn . prettyGroup) . snd
