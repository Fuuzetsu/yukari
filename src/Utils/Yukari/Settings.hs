{-# LANGUAGE TemplateHaskell #-}
module Utils.Yukari.Settings where

import Control.Lens
import Data.List (isPrefixOf)
import Utils.Yukari.Types



data YukariSettings = YukariSettings { _siteSettings :: SiteSettings
                                     , _spendSettings :: SpendSettings
                                     , _programSettings :: [ProgramSettings]
                                     , _logVerbosity :: Verbosity
                                     , _connectionRetries :: Integer
                                     , _maxPages :: Integer
                                     }

data SiteSettings = SiteSettings { _username :: String
                                 , _password :: String
                                 , _baseSite :: String
                                 , _loginSite :: String
                                 , _searchSite :: String
                                 , _topWatch :: Maybe FilePath
                                 , _watchFunc :: Category -> Maybe FilePath
                                 , _filterFunc :: ABTorrent -> Bool
                                 , _clobberFiles :: Bool
                                 , _groupPreprocessor :: ABTorrentGroup
                                                      -> ABTorrentGroup
                                 }

data SpendSettings = SpendSettings { _yenSite :: String
                                   , _yenLeftOver :: Integer
                                   }
data ProgramSettings = DryRun | SpendYen | DownloadTorrents
                     deriving (Show, Eq)

data Verbosity = Quiet | Low | High | Debug
               deriving (Show, Eq, Ord)


makeLenses ''YukariSettings
makeLenses ''SiteSettings
makeLenses ''SpendSettings


minimalSettings :: String -> String -> String -> SiteSettings
minimalSettings u p l = SiteSettings { _username = u
                                     , _password = p
                                     , _baseSite = ""
                                     , _loginSite = l
                                     , _searchSite = ""
                                     , _topWatch = Nothing
                                     , _watchFunc = const Nothing
                                     , _filterFunc = const True
                                     , _clobberFiles = False
                                     , _groupPreprocessor = id
                                     }

-- | Some defaults for the current AnimeBytes site
animebytesSettings :: YukariSettings
animebytesSettings = YukariSettings
  { _siteSettings = abSiteSettings
  , _spendSettings = abSpendSettings
  , _programSettings = []
  , _logVerbosity = Low
  , _connectionRetries = 3
  , _maxPages = 10
  }

abSiteSettings :: SiteSettings
abSiteSettings = SiteSettings
  { _username = ""
  , _password = ""
  , _baseSite = "https://animebytes.tv"
  , _loginSite = "https://animebytes.tv/login.php"
  , _searchSite = "https://animebytes.tv/torrents.php"
  , _topWatch = Nothing
  , _watchFunc = const Nothing
  , _filterFunc = const False
  , _clobberFiles = False
  , _groupPreprocessor = fixImageLinks
  }
  where
    -- Images nowadays seem to start with this weird link that can't be accessed
    -- from a browser unless we're on the page already and directly click on it.
    -- We fix that here.
    fixImageLinks :: ABTorrentGroup -> ABTorrentGroup
    fixImageLinks = torrentImageURI %~ \i -> if "//" `isPrefixOf` i
                                             then "https:" ++ i
                                             else i

abSpendSettings :: SpendSettings
abSpendSettings = SpendSettings { _yenSite = "https://animebytes.tv/konbini.php"
                                , _yenLeftOver = 0
                                }
