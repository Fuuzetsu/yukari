{-# LANGUAGE NoMonomorphismRestriction #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Utils.Yukari.ConfigParser (parseConfig)
import Utils.Yukari.Crawler
import Utils.Yukari.Filters
import Utils.Yukari.Settings
import Utils.Yukari.Spender
import Utils.Yukari.Types

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

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 0) $ do
    putStrLn $ "This program currently takes no arguments. "
      ++ "Populate your ~/.yukari instead"
    exitFailure
  progName <- getProgName
  c <- readConfig
  case c of
    Left e -> putStrLn $ "Error: " ++ e
    Right (spendS, _) -> spendYen spendS
 --spendSettings { regularSettings = (regularSettings spendSettings) { username = head args , password = last args }
 -- crawlFromURL $ searchSettings
 -- spendYen spendSettings
