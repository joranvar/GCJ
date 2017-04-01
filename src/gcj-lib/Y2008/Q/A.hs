module Y2008.Q.A where

import Protolude

import qualified Data.Text as Text (concat)
import Data.String

solve :: Text -> Text
solve =  Text.concat . map solve' . parse . drop 1 . lines . toS

data P = P { ss :: [String]
           , qs :: [String]
           }

parse :: [String] -> [P]
solve' :: P -> Text

parse [] = []
parse (s:rest) =
  let (ss', (q:rest')) = splitAt (maybe 0 fst $ head $ reads s) rest
      (qs', rest'') = splitAt (maybe 0 fst $ head $ reads q) rest'
  in P ss' qs' : parse rest''

solve' (P _ _) = "Done"
