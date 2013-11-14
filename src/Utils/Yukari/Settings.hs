module Utils.Yukari.Settings where

import Utils.Yukari.Types (Category, ABTorrent)
import System.FilePath
import Data.Maybe


data YukariSettings = YukariSettings { siteSettings :: SiteSettings
                                     , spendSettings :: SpendSettings
                                     , programSettings :: [ProgramSettings]
                                     , logVerbosity :: Verbosity
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
                                     }
