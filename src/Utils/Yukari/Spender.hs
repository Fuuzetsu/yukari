module Utils.Yukari.Spender (spendYen) where

import Data.List (intersperse)
import Utils.Yukari.Settings
import Utils.Yukari.Crawler (getSinglePage)
import Utils.Yukari.Parser (parseYenPage)
import Utils.Yukari.Types
import Control.Monad

spendYen :: YukariSettings -> IO ()
spendYen ys = do
  let rs = siteSettings ys
  let v = logVerbosity ys
  page <- parseYenPage =<< getSinglePage rs (yenSite $ spendSettings ys)
  verb Low ["Currently have", show $ yenOwned page, "yen."]
  verb High ["Leftover yen limit set to", show . yenLeftOver $ spendSettings ys]

  case chooseOptimal (spendSettings ys) (attachBase page) of
    Nothing -> when (v >= Low) (putStrLn "No viable spending found")
    Just (cost, link) -> do
      verb Low ["Spending", show cost, "yen."]
      verb High ["Spending the yen at", link]
      getSinglePage rs link >> spendYen ys
  where
    attachBase :: YenPage -> YenPage
    attachBase yp = yp { spendingLinks = map (\(c, l) -> (,) c (baseSite (siteSettings ys) ++ "/" ++ l)) (spendingLinks yp)}

    verb :: Verbosity -> [String] -> IO ()
    verb v s = when (v >= logVerbosity ys)
               (putStrLn . intersperse ' ' $ concat s)

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
