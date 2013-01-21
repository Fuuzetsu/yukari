module Formatter where

import Types

prettyGroup :: ABTorrentGroup -> String
prettyGroup g = torrentName g ++ " - " ++ show (torrentCategory g) ++ "\n" ++
                "Image: " ++ torrentImageURI g ++ "\nTags: " ++ unwords (map (++ " ") $ torrentTags g) ++ "\n"
                ++ unlines (map (indent.prettyTorrent) $ torrents g)
                where indent l = unlines $ map ("  \t" ++ ) $ lines l

prettyTorrent :: ABTorrent -> String
prettyTorrent t = torrentInfoSuffix t ++ "\tSize: " ++ show (torrentSize t) ++ "\tSeeders: " ++ show (torrentSeeders t)
                  ++ "\nLink: " ++ torrentURI t ++ "\nDownload: " ++ torrentDownloadURI t ++ "\n" ++ prettyInfo (torrentInfo t)

prettyResolution (Resolution w h) = show w ++ " x " ++ show h

prettyInfo :: Information -> String
prettyInfo i = case i of
                      NoInfo -> "No additional information."
                      AnimeInformation ai -> unlines [ "Format: " ++ show (releaseFormat ai)
                                                     , "Container: " ++ show (videoContainer ai)
                                                     , "Subtitles: " ++ prettySub (subtitles ai)
                                                     , "Resolution: " ++ prettyResolution (resolution ai)
                                                     , "Audio: " ++ show (audio ai)
                                                     ]
                      MangaInformation mi -> unlines [ "Scanlated: " ++ boolYesNo (scanlated mi)
                                                     , "Archived: " ++ boolYesNo (archived mi)
                                                     , "Ongoing: " ++ boolYesNo (ongoing mi)
                                                     ]

boolYesNo b = if b then "Yes" else "No"

prettySub :: Subtitles -> String
prettySub (Softsub x) = "Softsubbed by " ++ x
prettySub (Hardsub x) = "Hardsubbed by " ++ x
prettySub RAW = "None"

prettyPage :: (String, [ABTorrentGroup]) -> IO ()
prettyPage = mapM_ (putStrLn . prettyGroup) . snd