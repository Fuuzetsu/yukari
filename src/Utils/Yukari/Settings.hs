module Utils.Yukari.Settings where

import Data.List (isPrefixOf)
import Utils.Yukari.Types



data YukariSettings = YukariSettings { siteSettings :: SiteSettings
                                     , spendSettings :: SpendSettings
                                     , programSettings :: [ProgramSettings]
                                     , logVerbosity :: Verbosity
                                     , connectionRetries :: Integer
                                     , maxPages :: Integer
                                     }

data SiteSettings = SiteSettings { username :: String
                                 , password :: String
                                 , baseSite :: String
                                 , loginSite :: String
                                 , searchSite :: String
                                 , topWatch :: Maybe FilePath
                                 , watchFunc :: Category -> Maybe FilePath
                                 , filterFunc :: ABTorrent -> Bool
                                 , clobberFiles :: Bool
                                 , groupPreprocessor :: ABTorrentGroup
                                                     -> ABTorrentGroup
                                 }

data SpendSettings = SpendSettings { yenSite :: String
                                   , yenLeftOver :: Integer
                                   }

data ProgramSettings = DryRun | SpendYen | DownloadTorrents
                     deriving (Show, Eq)

data Verbosity = Quiet | Low | High | Debug
               deriving (Show, Eq, Ord)

minimalSettings :: String -> String -> String -> SiteSettings
minimalSettings u p l = SiteSettings { username = u
                                     , password = p
                                     , baseSite = ""
                                     , loginSite = l
                                     , searchSite = ""
                                     , topWatch = Nothing
                                     , watchFunc = const Nothing
                                     , filterFunc = const True
                                     , clobberFiles = False
                                     , groupPreprocessor = id
                                     }

-- | Some defaults for the current AnimeBytes site
animebytesSettings :: YukariSettings
animebytesSettings = YukariSettings
  { siteSettings = abSiteSettings
  , spendSettings = abSpendSettings
  , programSettings = []
  , logVerbosity = Low
  , connectionRetries = 3
  , maxPages = 10
  }

abSiteSettings :: SiteSettings
abSiteSettings = SiteSettings
  { username = ""
  , password = ""
  , baseSite = "https://animebytes.tv"
  , loginSite = "https://animebytes.tv/login.php"
  , searchSite = "https://animebytes.tv/torrents.php"
  , topWatch = Nothing
  , watchFunc = const Nothing
  , filterFunc = const False
  , clobberFiles = False
  , groupPreprocessor = fixImageLinks
  }
  where
    -- Images nowadays seem to start with this weird link that can't be accessed
    -- from a browser unless we're on the page already and directly click on it.
    -- We fix that here.
    fixImageLinks :: ABTorrentGroup -> ABTorrentGroup
    fixImageLinks g = let img = torrentImageURI g
                      in if "//" `isPrefixOf`img
                         then g { torrentImageURI = "https:" ++ img }
                         else g

abSpendSettings :: SpendSettings
abSpendSettings = SpendSettings { yenSite = "https://animebytes.tv/konbini.php"
                                , yenLeftOver = 0
                                }
