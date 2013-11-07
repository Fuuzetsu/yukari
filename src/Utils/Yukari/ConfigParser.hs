{-# LANGUAGE OverloadedStrings #-}
module Utils.Yukari.ConfigParser where

import           Control.Applicative
import           Data.List
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
        <|> fail ("Failed to parse the " ++ s ++ " field in the config file.")

usernameP :: P.Parser String
usernameP = kvP "username"

passwordP :: P.Parser String
passwordP = kvP "password"

logP :: P.Parser Verbosity
logP = do
  s <- kvP "logVerbosity"
  case s of
    "Quiet" -> return Quiet
    "Low" -> return Low
    "High" -> return High
    o -> fail $ o ++ " is not a valid verbosity level! "
         ++ "Choose Quiet, Low or High"

boolP :: String -> P.Parser Bool
boolP s = do
  b <- kvP s
  case b of
    "False" -> return False
    "True" -> return True
    o -> fail $ o ++ " is not a valid Bool value. Choose True or False"

configParser :: ConfParser
configParser = do
  u <- usernameP
  p <- passwordP
  bs <- kvP "baseSite"
  ls <- kvP "loginSite"
  ss <- kvP "searchSite"
  v <- logP
  tw <- kvP "topWatch"
  clobberFiles <- boolP "clobberFiles"
  let st = siteSettings { username = u
                        , password = p
                        }
  return (spendSettings, st)

parseConfig :: String -> Either String (SpendSettings, SiteSettings)
parseConfig s = case P.parseOnly configParser (T.pack s) of
  Left err -> if "Failed reading: " `isPrefixOf` err
              then Left $ drop (length ("Failed reading: " :: String)) err
              else Left err
  r -> r
