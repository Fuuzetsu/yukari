{-# LANGUAGE CPP #-}

module Yukari.ParseSpec (main, spec) where

import Control.Applicative
import Data.List
import Utils.Yukari.Parser
import Utils.Yukari.Settings
import Utils.Yukari.Types
import System.Directory
import System.FilePath
import Test.Hspec


yukariSettings :: YukariSettings
yukariSettings = YukariSettings
  { siteSettings = siteSettings'
  , spendSettings = spendSettings'
  , programSettings = [DryRun]
  , logVerbosity = Quiet
  , connectionRetries = 0
  }

spendSettings' :: SpendSettings
spendSettings' = SpendSettings { yenSite = "https://animebytes.tv/konbini.php"
                               , yenLeftOver = 999999999999
                               }

siteSettings' :: SiteSettings
siteSettings' = SiteSettings
  { username = "TestUser"
  , password = "TestPassword"
  , baseSite = "https://animebytes.tv"
  , loginSite = "https://animebytes.tv/login.php"
  , searchSite = "https://animebytes.tv/torrents.php"
  , topWatch = Nothing
  , watchFunc = const Nothing
  , filterFunc = const False
  , clobberFiles = False
  }

testDir :: FilePath
testDir = "test" </> "testfiles"

readHtml :: IO String
readHtml = readFile $ testDir </> "simple.html"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let parse = readHtml >>= parsePage yukariSettings
      f `pShouldReturn` x = f <$> parse `shouldReturn` x

  describe "when parsing an example torrent search page" $ do
    it "parses the right amount of groups" $ do
      length . snd <$> parse `shouldReturn` 7

    it "picks up all individual torrents" $ do
      length . concatMap torrents . snd <$> parse `shouldReturn` 12

    it "realises there's no next page" $ do
      fst <$> parse `shouldReturn` ""

    it "parses the right amount of oneshots" $ do
      countCategory (Manga Oneshot) `pShouldReturn` 2

    it "parses the right amount of live action movies" $ do
      countCategory LiveAction `pShouldReturn` 1

    it "parses the right amount of live action series" $ do
      countCategory LiveActionSeries `pShouldReturn` 2

    it "parses the right amount of manga" $ do
      countCategory (Manga GenericManga) `pShouldReturn` 2

    it "should not find any anime" $ do
      countCategory Anime `pShouldReturn` 0

    where
      count x = length . elemIndices x
      countCategory x = count x . map torrentCategory . snd
