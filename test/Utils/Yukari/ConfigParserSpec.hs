{-# LANGUAGE OverloadedStrings #-}
module Utils.Yukari.ConfigParserSpec (main, spec) where

import Data.Text
import Test.Hspec
import Data.Either
import Data.Attoparsec.Text
import Utils.Yukari.ConfigParser
import Utils.Yukari.Settings

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "config file parsing" $ do
    let configFile = "username = foo\npassword = bar\n"
    it "can parse out the username" $ do
      parseField username usernameP configFile `shouldBe` Right "foo"

    it "fails properly when it can't parse the username field" $ do
      parseField username usernameP  "invalidfield = foo\n"
        `shouldBe` Left "Failed to parse the username field in the config file."

    it "can parse out the password" $ do
      parseField password passwordP configFile `shouldBe` Right "bar"

    it "fails properly when it can't parse the password field" $ do
      parseField password passwordP  "username = bar\ninvalidfield = foo\n"
        `shouldBe` Left "Failed to parse the password field in the config file."


parseField :: (SiteSettings -> a) -> Parser a -> String -> Either String a
parseField f p s = case parseConfig s of
  Left e -> Left e
  Right (sp, st) -> Right $ f st
