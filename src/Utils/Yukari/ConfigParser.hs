{-# LANGUAGE OverloadedStrings #-}
module Utils.Yukari.ConfigParser where

import Control.Applicative
import Debug.Trace
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import           System.Directory
import           System.FilePath
import           Utils.Yukari.Filters
import           Utils.Yukari.Settings
import           Utils.Yukari.Types

type ConfParser = P.Parser (SpendSettings, SiteSettings)

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


siteSettings = SiteSettings { username = ""
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

spendSettings = SpendSettings { yenSite = "https://animebytes.tv/konbini.php"
                              , yenLeftOver = 0
                              }


torrentFilter :: ABTorrent -> Bool
torrentFilter tor = all (`id` tor) filters
                    where filters = [ isSeeded
                                    , isUnderSize (100 * 1024 ^ 2)
                                    ]

kvP :: String -> P.Parser String
kvP s = T.unpack <$> (P.string (T.pack $ s ++ " = ")
                      *> P.takeWhile (/= '\n') <* "\n")

usernameP :: P.Parser String
usernameP = kvP "username"

passwordP :: P.Parser String
passwordP = kvP "password"

configParser :: ConfParser
configParser = do
  u <- kvP "username"
  p <- kvP "password"
  let st = siteSettings { username = u
                        , password = p
                        }
  return (spendSettings, st)

parseConfig :: String -> Either String (SpendSettings, SiteSettings)
parseConfig s = P.parseOnly configParser (T.pack s)
