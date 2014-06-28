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
  , maxPages = 999999
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
  , groupPreprocessor = id
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

    it "should find two torrents with softsubs" $ do
      countSub softSub `pShouldReturn` 3

    it "should find three torrents with hardsubs" $ do
      countSub hardSub `pShouldReturn` 3

    it "should find one torrent with softsubs by WiKi" $ do
      countSub (== Softsub "WiKi") `pShouldReturn` 1

    it "should find two torrents with hardsubs by sars" $ do
      countSub (== Hardsub "sars") `pShouldReturn` 2

    it "should find one torrent with hardsubs by d-addicts" $ do
      countSub (== Hardsub "d-addicts") `pShouldReturn` 1

    it "should find one torrent with softsubs by d-addicts" $ do
      countSub (== Softsub "d-addicts") `pShouldReturn` 1

    it "should find one torrent with softsubs by an unspecified group" $ do
      countSub (== Softsub "") `pShouldReturn` 1

    it "should find three torrents with MP3 audio" $ do
      countAudio (== MP3) `pShouldReturn` 3

    it "should find two torrents with AAC audio" $ do
      countAudio (== AAC) `pShouldReturn` 2

    it "should find one torrent with DTS 5.1 audio" $ do
      countAudio (== DTS "5.1") `pShouldReturn` 1

    where
      softSub (Softsub _) = True
      softSub _ = False

      hardSub (Hardsub _) = True
      hardSub _ = False

      count x = length . elemIndices x
      countCategory x = count x . map torrentCategory . snd

      getInfo = map torrentInfo . concatMap torrents . snd

      getAudio p = audio' p . getInfo
        where
          audio' :: (Audio -> Bool) -> [Information] -> [Audio]
          audio' p info =
            let audioInfo  = [ audio x | AnimeInformation x <- info ]
            in filter p audioInfo

      countAudio p = length . getAudio p

      getSubs p = subs p . getInfo
        where
          subs :: (Subtitles -> Bool) -> [Information] -> [Subtitles]
          subs p info =
            let aniInfo = [ subtitles x | AnimeInformation x <- info ]
            in filter p aniInfo

      countSub p = length . getSubs p
