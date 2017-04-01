module Y2008.R1A.B where

import Protolude

import qualified Data.Text as Text (concat)
import Data.List.Split
import Data.String

solve :: Text -> Text
solve =  Text.concat . map write . zip [1..] .  map solve' . parse . drop 1 . lines . toS

data P = P { n :: Int
           , ms :: [[(Int, Bool)]] }
  deriving Show

data S = S [Bool] | Impossible

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, S) -> Text

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads

parse (n':m:rest) =
  let (ms', rest') = splitAt (read 0 m) rest
  in P (read 0 n') (map (map (read (0, False)) . pairWise . chunksOf 2 . drop 1 . words) ms') : parse rest'
  where
    pairWise :: [[String]] -> [String]
    pairWise ([a, b]:rest') = ("(" ++ a ++ "," ++ if b == "0" then "False" else "True" ++ ")") : pairWise rest'
    pairWise _ = []
parse _ = []

{-> parse . drop 1 . lines $ "2\n5\n3\n1 1 1\n2 1 0 2 0\n1 5 0\n1\n2\n1 1 0\n1 1 1\n"

[P {n = 5, ms = [[(1,True)],[(0,False),(0,False)],[(0,False)]]},P {n = 1, ms = [[(0,False)],[(1,True)]]}]
-}

solve' P{..} = Impossible

write (i, Impossible) = toS $ "Case #" ++ show i ++ ": IMPOSSIBLE\n"
write (i, S ss) = toS $ "Case #" ++ show i ++ ":" ++ concatMap (\s -> " " ++ if s then "1" else "0") ss ++ "\n"
