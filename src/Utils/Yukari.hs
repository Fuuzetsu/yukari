{-# LANGUAGE NoMonomorphismRestriction #-}
module Utils.Yukari where

import qualified Config.Dyre as Dyre
import           Config.Dyre.Relaunch
import           Control.Applicative
import           Control.Monad
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           Utils.Yukari.Crawler
import           Utils.Yukari.Filters
import           Utils.Yukari.Settings
import           Utils.Yukari.Spender
import           Utils.Yukari.Types

showError :: Maybe YukariSettings -> String -> Maybe YukariSettings
showError = const

dps = Dyre.defaultParams
  { Dyre.projectName = "Yukari"
  , Dyre.realMain = realMain
  , Dyre.showError = showError
  , Dyre.configCheck = True
  , Dyre.configDir = Just $ do hd <- getHomeDirectory
                               return $ hd </> ".yukari"
  }

dyreParams = Dyre.wrapMain dps


realMain :: Maybe YukariSettings -> IO ()
realMain Nothing = do
  putStrLn "Got Nothing as settings. This is bad."
  exitFailure
realMain (Just (YukariSettings site spend)) = do
  args <- getArgs
  when (length args /= 0) $ do
    putStrLn $ "This program currently takes no arguments. "
      ++ "Populate your ~/.yukari/Yukari.hs instead"
    exitFailure
  spendYen site spend

 -- crawlFromURL $ searchSettings
 -- spendYen spendSettings
