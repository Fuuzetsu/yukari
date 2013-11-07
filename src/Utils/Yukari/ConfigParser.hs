module Utils.Yukari.ConfigParser (parseConfig) where

import Data.Attoparsec.Text
import System.Directory
import System.FilePath
import Utils.Yukari.Filters
import Utils.Yukari.Settings
import Utils.Yukari.Types

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


parseConfig :: String -> Either String (SpendSettings, SiteSettings)
parseConfig = undefined
