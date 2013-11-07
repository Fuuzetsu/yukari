{-# LANGUAGE OverloadedStrings #-}
module Utils.Yukari.ConfigParserSpec (main, spec) where

import Data.Text
import Test.Hspec
import Data.Either
import Data.Attoparsec.Text
import Utils.Yukari.ConfigParser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "foo" $ do
    it "takes what I toss it" $ do
      parseOnly (string "end") "end" `shouldBe` Right "end"
