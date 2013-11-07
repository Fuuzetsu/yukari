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
import           Utils.Yukari.ConfigParser (parseConfig)
import           Utils.Yukari.Crawler
import           Utils.Yukari.Filters
import           Utils.Yukari.Settings
import           Utils.Yukari.Spender
import           Utils.Yukari.Types

readConfig :: IO (Either String (SpendSettings, SiteSettings))
readConfig = do
  configPath <- configFile
  t <- doesFileExist configPath
  if t
    then do
      c <- readFile configPath
      return $ parseConfig c
    else return $ Left ("The " ++ configPath ++ " config doesn't exist!")
  where
    configFile :: IO FilePath
    configFile = liftM (</> ".yukari") getHomeDirectory

showError :: Maybe YukariSettings -> String -> Maybe YukariSettings
showError = const

dyreParams = Dyre.wrapMain $ Dyre.defaultParams
  { Dyre.projectName = "Yukari"
  , Dyre.realMain = realMain
  , Dyre.showError = showError
  , Dyre.configDir = Just $ do hd <- getHomeDirectory
                               return $ hd </> ".yukari"
  }


realMain :: Maybe YukariSettings -> IO ()
realMain Nothing = do
  putStrLn "Got Nothing as settings!"
  exitFailure
realMain (Just (YukariSettings site spend)) = do
  args <- getArgs
  when (length args /= 0) $ do
    putStrLn $ "This program currently takes no arguments. "
      ++ "Populate your ~/.yukari/Yukari.hs instead"
    exitFailure
  -- progName <- getProgName
  spendYen site spend

 -- crawlFromURL $ searchSettings
 -- spendYen spendSettings
