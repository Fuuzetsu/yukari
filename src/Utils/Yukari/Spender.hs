module Utils.Yukari.Spender (spendYen) where

import Data.List (intersperse)
import Utils.Yukari.Settings
import Utils.Yukari.Crawler (getSinglePage)
import Utils.Yukari.Formatter
import Utils.Yukari.Parser (parseYenPage)
import Utils.Yukari.Types
import Control.Monad

spendYen :: YukariSettings -> IO ()
spendYen ys = do
  let rs = siteSettings ys
  page <- parseYenPage =<< getSinglePage rs (yenSite $ spendSettings ys)
  verbPrint Low ys ["Currently have", show $ yenOwned page, "yen."]
  verbPrint High ys ["Leftover yen limit set to", show . yenLeftOver $ spendSettings ys]

  case chooseOptimal (spendSettings ys) (attachBase page) of
    Nothing -> verbPrint Low ys ["No viable spending option found"]
    Just (cost, link) -> do
      verbPrint Low ys ["Spending", show cost, "yen."]
      verbPrint High ys ["Spending the yen at", link]
      getSinglePage rs link >> spendYen ys
  where
    attachBase :: YenPage -> YenPage
    attachBase yp = yp { spendingLinks = map (\(c, l) -> (,) c (baseSite (siteSettings ys) ++ "/" ++ l)) (spendingLinks yp)}


chooseOptimal :: SpendSettings -> YenPage -> Maybe (Cost, String)
chooseOptimal ys yp = let l = filterUnwanted ys yp
                      in case l of
                        [] -> Nothing
                        -- [(c, s)] -> Just (c, s)
                        _ -> Just $ foldl1 (\x@(c, q) y@(d, w) -> if c >= d
                                                                  then x
                                                                  else y) l
  where
    filterUnwanted st pg = filter (\(c, _) -> yenOwned pg - c >= 0 && yenOwned pg - c >= yenLeftOver st) (spendingLinks pg)
