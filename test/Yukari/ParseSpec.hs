{-# LANGUAGE CPP #-}

module Yukari.ParseSpec (main, spec) where

import Control.Applicative
import Data.List
import Utils.Yukari.Filters
import Utils.Yukari.Parser
import Utils.Yukari.Settings
import Utils.Yukari.Types
import System.FilePath
import Test.Hspec


yukariSettings :: YukariSettings
yukariSettings = YukariSettings
  { _siteSettings = siteSettings'
  , _spendSettings = spendSettings'
  , _programSettings = [DryRun]
  , _logVerbosity = Quiet
  , _connectionRetries = 0
  , _maxPages = 999999
  }

spendSettings' :: SpendSettings
spendSettings' = SpendSettings { _yenSite = "https://animebytes.tv/konbini.php"
                               , _yenLeftOver = 999999999999
                               }

siteSettings' :: SiteSettings
siteSettings' = SiteSettings
  { _username = "TestUser"
  , _password = "TestPassword"
  , _baseSite = "https://animebytes.tv"
  , _loginSite = "https://animebytes.tv/login.php"
  , _searchSite = "https://animebytes.tv/torrents.php"
  , _topWatch = Nothing
  , _watchFunc = const Nothing
  , _filterFunc = const False
  , _clobberFiles = False
  , _groupFilterFunc = const True
  , _groupPreprocessor = id
  }

testDir :: FilePath
testDir = "test" </> "testfiles"

readHtml :: IO String
readHtml = readFile $ testDir </> "simple.html"

main :: IO ()
main = hspec spec

parse :: IO (String, [ABTorrentGroup])
parse = readHtml >>= parsePage yukariSettings

spec :: Spec
spec = do
  let f `pShouldReturn` x = f <$> parse `shouldReturn` x

  describe "when parsing an example torrent search page" $ do
    it "parses the right amount of groups" $ do
      length . snd <$> parse `shouldReturn` 7

    it "picks up all individual torrents" $ do
      length . concatMap _torrents . snd <$> parse `shouldReturn` 12

    it "finds right number of  unseeded torrents" $ do
      length . filter isUnseeded . concatMap _torrents . snd <$> parse
        `shouldReturn` 1

    it "finds right number of seeded torrents" $ do
      length . filter isSeeded . concatMap _torrents . snd <$> parse
        `shouldReturn` 11

    it "realises there's no next page" $ do
      fst <$> parse `shouldReturn` ""

    it "parses the right amount of oneshots" $ do
      countCategory (Just $ Manga Oneshot) `pShouldReturn` 2

    it "parses the right amount of live action movies" $ do
      countCategory (Just LiveAction) `pShouldReturn` 1

    it "parses the right amount of live action series" $ do
      countCategory (Just LiveActionSeries) `pShouldReturn` 2

    it "parses the right amount of manga" $ do
      countCategory (Just $ Manga GenericManga) `pShouldReturn` 2

    it "should not find any anime" $ do
      countCategory (Just Anime) `pShouldReturn` 0

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

      countCategory x = count x . map _torrentCategory . snd

      getInfo = map _torrentInfo . concatMap _torrents . snd

      getAudio p = audio' p . getInfo
        where
          audio' :: (Audio -> Bool) -> [Maybe Information] -> [Audio]
          audio' p' info =
            let audioInfo  = [ _audio x | Just (AnimeInformation x) <- info ]
            in filter p' audioInfo

      countAudio p = length . getAudio p

      getSubs p = subs p . getInfo
        where
          subs :: (Subtitles -> Bool) -> [Maybe Information] -> [Subtitles]
          subs p' info =
            let aniInfo = [ _subtitles x | Just (AnimeInformation x) <- info ]
            in filter p' aniInfo

      countSub p = length . getSubs p
