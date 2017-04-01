module Y2008.Q.A where

import Protolude

import qualified Data.Text as Text (concat)
import qualified Data.Set as Set
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

solve' P{..} = S . snd . foldl (countSets) (Set.empty, 0) . filter (`elem` ss) $ qs
  where
    countSets :: (Set String, Int) -> String -> (Set String, Int)
    countSets (qs', i) q | q `elem` qs' = (qs', i)
    countSets (qs', i) q | (length qs' == length ss - 1) = (Set.singleton q, i+1)
    countSets (qs', i) q = (Set.insert q qs', i)

write (i, S s) = toS $ "Case #" ++ show i ++ ": " ++ show s ++ "\n"
