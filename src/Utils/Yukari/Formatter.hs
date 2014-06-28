module Utils.Yukari.Formatter where

import Control.Monad (when)
import Utils.Yukari.Types
import Utils.Yukari.Settings

verbPrint :: Verbosity -> YukariSettings -> [String] -> IO ()
verbPrint v ys s = when (v <= logVerbosity ys) (putStrLn $ unwords s)

prettyGroup :: ABTorrentGroup -> String
prettyGroup g = torrentName g ++ " - " ++ show (torrentCategory g) ++ "\n"
                ++ "Image: " ++ torrentImageURI g
                ++ "\nTags: " ++ unwords (map (++ " ") $ torrentTags g)
                ++ "\n" ++ unlines (map (indent.prettyTorrent) $ torrents g)
  where indent l = unlines $ map ("  \t" ++ ) $ lines l

prettyTorrent :: ABTorrent -> String
prettyTorrent t = torrentInfoSuffix t
                  ++ "\tSize: " ++ show (torrentSize t)
                  ++ "\tSeeders: " ++ show (torrentSeeders t)
                  ++ "\nLink: " ++ torrentURI t
                  ++ "\nDownload: " ++ torrentDownloadURI t
                  ++ "\n" ++ prettyInfo (torrentInfo t)

prettyResolution :: Maybe Resolution -> String
prettyResolution Nothing = "Unknown"
prettyResolution (Just (Resolution w h)) = show w ++ " x " ++ show h

showM :: Show a => Maybe a -> String
showM Nothing = "Unknown"
showM (Just a) = show a

prettyInfo :: Information -> String
prettyInfo i = case i of
  NoInfo -> "No additional information."
  AnimeInformation ai -> unlines
                         [ "Format: " ++ showM (releaseFormat ai)
                         , "Container: " ++ showM (videoContainer ai)
                         , "Subtitles: " ++ prettySub (subtitles ai)
                         , "Resolution: " ++ prettyResolution (resolution ai)
                         , "Audio: " ++ show (audio ai)
                         ]
  MangaInformation mi -> unlines [ "Scanlated: " ++ boolYesNo (scanlated mi)
                                 , "Archived: " ++ boolYesNo (archived mi)
                                 , "Ongoing: " ++ boolYesNo (ongoing mi)
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
