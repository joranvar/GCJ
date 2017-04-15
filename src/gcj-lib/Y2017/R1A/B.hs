module Y2017.R1A.B where

import Protolude

import Data.String

import GCJ

parse :: [String] -> [P]
solve' :: P -> S
write :: S -> Text
verify :: P -> S -> Verification
solve :: Bool -> Text -> Text
solve = GCJ.solve (Parse parse) (Solve solve') (Write write) (Verify verify)

example :: String
example = "6\n2 1\n500 300\n900\n660\n2 1\n500 300\n1500\n809\n2 2\n50 100\n450 449\n1100 1101\n2 1\n500 300\n300\n500\n1 8\n10\n11 13 17 11 16 14 12 18\n3 3\n70 80 90\n1260 1500 700\n800 1440 1600\n1700 1620 900"

data P = P { ingredients::[(Int,[Int])]  } deriving Show
data S = S Int deriving Show

parse (np:rs':rest) =
  let (n:_) = map (read 0) $ words np
      rs = map (read 0) $ words rs'
      (ps', rest') = splitAt n rest
      ps = map (map (read 0) . words) $ ps'
  in P (zip rs ps) : parse rest'
parse _ = []

{-> parse . drop 1 . lines $ example

[P {ingredients = [(500,[900]),(300,[660])]},P {ingredients = [(500,[1500]),(300,[809])]},P {ingredients = [(50,[450,449]),(100,[1100,1101])]},P {ingredients = [(500,[300]),(300,[500])]},P {ingredients = [(10,[11,13,17,11,16,14,12,18])]},P {ingredients = [(70,[1260,1500,700]),(80,[800,1440,1600]),(90,[1700,1620,900])]}]
-}

solve' P{..} = S 0

{-> map solve' $ parse . drop 1 . lines $ example

[]
-}

write (S n) = toS $ " " ++ show n ++ "\n"

verify P{..} (S _) = Nothing
