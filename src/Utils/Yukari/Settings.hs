module Utils.Yukari.Settings where

import Utils.Yukari.Types (Category, ABTorrent)
import System.FilePath
import Data.Maybe


data YukariSettings = YukariSettings SiteSettings SpendSettings

data SiteSettings = SiteSettings { username :: String
                                 , password :: String
                                 , baseSite :: String
                                 , loginSite :: String
                                 , searchSite :: String
                                 , topWatch :: Maybe FilePath
                                 , logVerbosity :: Verbosity
                                 , watchFunc :: Category -> Maybe FilePath
                                 , filterFunc :: ABTorrent -> Bool
                                 , clobberFiles :: Bool
                                 }

data SpendSettings = SpendSettings { yenSite :: String
                                   , yenLeftOver :: Integer
                                   }

data Verbosity = Quiet | Low | High deriving (Show, Eq, Ord)

minimalSettings :: String -> String -> String -> SiteSettings
minimalSettings u p l = SiteSettings { username = u
                                     , password = p
                                     , baseSite = ""
                                     , loginSite = l
                                     , searchSite = ""
                                     , topWatch = Nothing
                                     , logVerbosity = Low
                                     , watchFunc = const Nothing
                                     , filterFunc = const True
                                     , clobberFiles = False
                                     }
