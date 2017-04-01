module Y2008.Q.A where

import Protolude

import qualified Data.Text as Text (concat)
import Data.String

solve :: Text -> Text
solve =  Text.concat . map write . zip [1..] .  map solve' . parse . drop 1 . lines . toS

data P = P { ss :: [String]
           , qs :: [String]
           }

data S = S Int

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, S) -> Text

parse [] = []
parse (s:rest) =
  let (ss', (q:rest')) = splitAt (maybe 0 fst $ head $ reads s) rest
      (qs', rest'') = splitAt (maybe 0 fst $ head $ reads q) rest'
  in P ss' qs' : parse rest''

solve' (P _ _) = S 1

write (i, S s) = toS $ "Case #" ++ show i ++ ": " ++ show s ++ "\n"
