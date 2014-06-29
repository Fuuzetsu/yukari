module Utils.Yukari.Filters where

import Utils.Yukari.Types
import Control.Monad

isSeeded :: ABTorrent -> Bool
isSeeded = (0 <) . _torrentSeeders

isUnseeded :: ABTorrent -> Bool
isUnseeded = not . isSeeded

isSize :: Integer -> ABTorrent -> Bool
isSize bytes = liftM2 (==) (isUnderSize bytes) (isOverSize bytes)

isUnderSize :: Integer -> ABTorrent -> Bool
isUnderSize bytes = (>) bytes . _torrentSize

isOverSize :: Integer -> ABTorrent -> Bool
isOverSize bytes = (<) bytes . _torrentSize
