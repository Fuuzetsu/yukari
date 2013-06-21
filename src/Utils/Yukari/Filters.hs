module Utils.Yukari.Filters where

import Utils.Yukari.Types
import Control.Monad

isSeeded :: ABTorrent -> Bool
isSeeded = (<) 0 . torrentSeeders

isUnseeded :: ABTorrent -> Bool
isUnseeded = not . isSeeded

isSize :: Integer -> ABTorrent -> Bool
isSize bytes = liftM2 (==) (isUnderSize bytes) (isOverSize bytes)

isUnderSize :: Integer -> ABTorrent -> Bool
isUnderSize bytes = (>) bytes . torrentSize

isOverSize :: Integer -> ABTorrent -> Bool
isOverSize bytes = (<) bytes . torrentSize


