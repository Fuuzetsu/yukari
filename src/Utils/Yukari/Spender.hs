module Utils.Yukari.Spender (spendYen) where

import Utils.Yukari.Settings
import Utils.Yukari.Crawler (getSinglePage)
import Utils.Yukari.Parser (parseYenPage)
import Utils.Yukari.Types
import Control.Monad

spendYen :: SpendSettings -> IO ()
spendYen ys = do
  let rs = regularSettings ys
  let v = logVerbosity rs
  page <- parseYenPage =<< getSinglePage rs (yenSite ys)
  when (v >= Low) (putStrLn $ "Currently have " ++ show (yenOwned page) ++ " yen.")
  when (v >= High) (putStrLn $ "Left-over yen limit set to " ++ show (yenLeftOver ys))
  case chooseOptimal ys (attachBase page) of
    Nothing -> when (v >= Low) (putStrLn "No viable spending found")
    Just (cost, link) -> do
      when (v >= Low) (putStrLn $ "Spending " ++ show cost ++ " yen.")
      when (v >= High) (putStrLn $ "Spending the yen at " ++ link)
      getSinglePage (regularSettings ys) link >> spendYen ys
  where attachBase yp = yp { spendingLinks = map (\(c, l) -> (,) c (baseSite (regularSettings ys) ++ "/" ++ l)) (spendingLinks yp)}

chooseOptimal :: SpendSettings -> YenPage -> Maybe (Cost, String)
chooseOptimal ys yp = let l = filterUnwanted ys yp in if null l then Nothing else Just $ foldl (\x@(c, q) y@(d, w) -> if c >= d then x else y) (head l) (tail l)
  where filterUnwanted st pg = filter (\(c, _) -> yenOwned pg - c >= 0 && yenOwned pg - c >= yenLeftOver st) (spendingLinks pg)
