module Utils.Yukari.Spender (spendYen) where

import Control.Arrow (second)
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
  verbPrint High ys ["Leftover yen limit set to"
                    , show . yenLeftOver $ spendSettings ys]

  case chooseOptimal (spendSettings ys) (attachBase page) of
    Nothing -> verbPrint Low ys ["No viable spending option found"]
    Just (cost, link) -> do
      verbPrint Low ys ["Spending", show cost, "yen."]
      verbPrint High ys ["Spending the yen at", link]
      getSinglePage rs link >> spendYen ys
  where
    -- Attach the partial links we get from the site with
    -- the full link of the base site, adding the ‘/’ separator
    attachBase :: YenPage -> YenPage
    attachBase yp =
      let bs = baseSite (siteSettings ys) ++ "/"
      in yp { spendingLinks = map (second (bs ++)) (spendingLinks yp) }


chooseOptimal :: SpendSettings -> YenPage -> Maybe (Cost, String)
chooseOptimal ys yp = let l = filterUnwanted ys yp
                      in case l of
                        [] -> Nothing
                        _ -> Just $ foldl1 (\x@(c, q) y@(d, w) -> if c >= d
                                                                  then x
                                                                  else y) l
  where
    filterUnwanted :: SpendSettings -> YenPage -> [(Cost, String)]
    filterUnwanted st pg =
      let yOwned' = yenOwned pg
          yenLeft' = yenLeftOver st
          links = spendingLinks pg
      in filter (\(c, _) -> yOwned' - c >= 0 && yOwned' - c >= yenLeft') links
