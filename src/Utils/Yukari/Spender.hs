module Utils.Yukari.Spender (spendYen) where

import Control.Arrow (second)
import Control.Lens
import Utils.Yukari.Settings
import Utils.Yukari.Crawler (getSinglePage)
import Utils.Yukari.Formatter
import Utils.Yukari.Parser (parseYenPage)
import Utils.Yukari.Types

spendYen :: YukariSettings -> IO ()
spendYen ys = do
  body <- getSinglePage ys (ys ^. spendSettings . yenSite)
  case body of
    Nothing -> putStrLn $ "Failed to fetch " ++ ys ^. spendSettings . yenSite
    Just x -> do
      page <- parseYenPage x
      verbPrint Low ys ["Currently have", show $ page ^. yenOwned, "yen."]
      verbPrint High ys ["Leftover yen limit set to"
                        , show $ ys ^. spendSettings . yenLeftOver]

      case chooseOptimal (ys ^. spendSettings) (attachBase page) of
        Nothing -> verbPrint Low ys ["No viable spending option found"]
        Just (cost, link) -> do
          verbPrint Low ys ["Spending", show cost, "yen."]
          verbPrint High ys ["Spending the yen at", link]
          getSinglePage ys link >> spendYen ys
  where
    -- Attach the partial links we get from the site with
    -- the full link of the base site, adding the ‘/’ separator
    attachBase :: YenPage -> YenPage
    attachBase yp =
      let bs = ys ^. siteSettings . baseSite ++ "/"
      in yp & spendingLinks %~ map (second (bs ++))


chooseOptimal :: SpendSettings -> YenPage -> Maybe (Cost, String)
chooseOptimal ys yp = let l = filterUnwanted ys yp
                      in case l of
                        [] -> Nothing
                        _ -> Just $ foldl1 (\x@(c, _) y@(d, _) -> if c >= d
                                                                  then x
                                                                  else y) l
  where
    filterUnwanted :: SpendSettings -> YenPage -> [(Cost, String)]
    filterUnwanted st pg =
      let yOwned' = pg ^. yenOwned
          yenLeft' = st ^. yenLeftOver
          links = pg ^. spendingLinks
      in filter (\(c, _) -> yOwned' - c >= 0 && yOwned' - c >= yenLeft') links
