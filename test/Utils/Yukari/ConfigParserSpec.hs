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

    it "can parse out the password" $ do
      parseField password passwordP configFile `shouldBe` Right "bar"


parseField :: (SiteSettings -> a) -> Parser a -> String -> Either String a
parseField f p s = case parseConfig s of
  Left e -> Left e
  Right (sp, st) -> Right $ f st
