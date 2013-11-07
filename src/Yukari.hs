{-# LANGUAGE NoMonomorphismRestriction #-}
module Main (main) where

import Utils.Yukari.Crawler
import Utils.Yukari.Settings
import Utils.Yukari.Types
import Utils.Yukari.Filters
import Utils.Yukari.Spender
import System.FilePath
import System.Environment
import System.Exit

searchSettings = SiteSettings { username = ""
                              , password = ""
                              , baseSite = "https://animebytes.tv"
                              , loginSite = "https://animebytes.tv/login.php"
                              , searchSite = "https://animebytes.tv/torrents.php"
                              , logVerbosity = Low
                              , topWatch = Just $ "/mnt" </> "hitagi" </> "watch"
                              , watchFunc = watchDirs
                              , filterFunc = torrentFilter
                              , clobberFiles = False
                              }

spendSettings = SpendSettings { regularSettings = searchSettings
                              , yenSite = "https://animebytes.tv/konbini.php"
                              , yenLeftOver = 0
                              }

torrentFilter :: ABTorrent -> Bool
torrentFilter tor = all (`id` tor) filters
                    where filters = [ isSeeded
                                    , isUnderSize (100 * 1024 ^ 2)
                                    ]

watchDirs :: Category -> Maybe FilePath
watchDirs cat
  | cat == Anime = Just "watchanime"
  | cat == Artbook = Just "watchartbooks"
  | cat `elem` [Game, GameGuide] = Just "watchgames"
  | cat `elem` [LightNovel, Novel, Anthology] = Just "watchnovels"
  | cat == VisualNovel = Just "watchvnovels"
  | cat `elem` map Music [Single, Soundtrack, MusicDVD, Live, PV, LiveAlbum
                         , Compilation, Album, DramaCD] = Just "watchmusic"
  | cat `elem` map Manga [GenericManga, Oneshot
                         , Manhua, Manhwa, OEL] = Just "watchmanga"
  | otherwise = Nothing


main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  if length args /= 2
    then putStrLn ("usage: " ++ progName ++ " <username> <password>") >> (exitWith $ ExitFailure 1)
    else spendYen $ spendSettings { regularSettings = (regularSettings spendSettings) { username = head args
                                                                                      , password = last args
                                                                                      }
                                 }
 -- crawlFromURL $ searchSettings
 -- spendYen spendSettings
