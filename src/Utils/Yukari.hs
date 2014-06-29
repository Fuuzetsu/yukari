{-# LANGUAGE NoMonomorphismRestriction #-}
module Utils.Yukari where

import qualified Config.Dyre as Dyre
import           Control.Lens
import           Control.Monad
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           Utils.Yukari.Crawler
import           Utils.Yukari.Formatter
import           Utils.Yukari.Settings
import           Utils.Yukari.Spender

showError :: Maybe YukariSettings -> String -> Maybe YukariSettings
showError = const

dps :: Dyre.Params (Maybe YukariSettings)
dps = Dyre.defaultParams
  { Dyre.projectName = "Yukari"
  , Dyre.realMain = realMain
  , Dyre.showError = showError
  , Dyre.configCheck = True
  , Dyre.configDir = Just $ do hd <- getHomeDirectory
                               return $ hd </> ".yukari"
  }

dyreParams :: Maybe YukariSettings -> IO ()
dyreParams = Dyre.wrapMain dps

realMain :: Maybe YukariSettings -> IO ()
realMain Nothing = do
  putStrLn "Please set up your ~/.yukari/Yukari.hs"
  exitFailure
realMain (Just ys) = do
  let ps = ys ^. programSettings
  args <- getArgs
  unless (null args) $ do
    putStrLn $ "This program currently takes no arguments. "
      ++ "Populate your ~/.yukari/Yukari.hs instead"
    exitFailure

  when (SpendYen `elem` ps) $ do
    verbPrint Low ys ["Spending yen"]
    spendYen ys

  when (DownloadTorrents `elem` ps) $ do
    verbPrint Low ys ["Dowloading torrents"]
    crawlFromURL ys
