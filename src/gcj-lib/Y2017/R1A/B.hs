module Y2017.R1A.B where

import Protolude

import Control.Arrow ((&&&))
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

data P = P { ingredients::[(Int,[Int])], p::Int } deriving Show
data S = S Int deriving Show

parse (np:rs':rest) =
  let (n:p:_) = map (read 0) $ words np
      rs = map (read 0) $ words rs'
      (ps', rest') = splitAt n rest
      ps = map (map (read 0) . words) $ ps'
  in P (zip rs ps) p : parse rest'
parse _ = []

{-> parse . drop 1 . lines $ example


<interactive>:1325:1: error:
    * Variable not in scope: parse
    * Perhaps you meant one of these:
        data constructor `Parse' (imported from GCJ),
        `parser' (imported from GCJ)
<interactive>:1325:7: error:
    Variable not in scope: (.) :: t2 -> t3 -> t1
<interactive>:1325:9: error:
    Variable not in scope:
      drop :: integer-gmp-1.0.0.1:GHC.Integer.Type.Integer -> t3
<interactive>:1325:16: error:
    Variable not in scope: (.) :: t1 -> t4 -> t0
<interactive>:1325:18: error: Variable not in scope: lines
<interactive>:1325:24: error:
    Variable not in scope: ($) :: t0 -> t5 -> t
<interactive>:1325:26: error: Variable not in scope: example
-}

solve' P{..} =
  let scale :: Int -> [(Double, Double)]
      scale n = map ((((* 0.9) &&& (* (1.1 :: Double))) . fromIntegral . (* n)) . fst) ingredients -- n servings
      between :: ((Double, Double), Int) -> Bool
      between ((atLeast, atMost), amounts) = ((\am -> am >= atLeast && am <= atMost) . fromIntegral) amounts
      isKit :: [Int] -> Bool
      isKit amounts = not . null $ filter (all between) . map (flip zip amounts) $ map scale [0..(minimum $ concatMap snd ingredients)]
  in S . maximum $ 0:(map (length . filter isKit) $ (:[]) $ oneEach $ map snd ingredients)

{-> map (oneEach . map snd . ingredients) $ parse . drop 1 . lines $ example

[[[900,660]],[[1500,809]],[[450,1100],[449,1100],[450,1101],[449,1101]],[[300,500]],[[11],[13],[17],[11],[16],[14],[12],[18]],[[1260,800,1700],[1500,800,1700],[700,800,1700],[1260,1440,1700],[1500,1440,1700],[700,1440,1700],[1260,1600,1700],[1500,1600,1700],[700,1600,1700],[1260,800,1620],[1500,800,1620],[700,800,1620],[1260,1440,1620],[1500,1440,1620],[700,1440,1620],[1260,1600,1620],[1500,1600,1620],[700,1600,1620],[1260,800,900],[1500,800,900],[700,800,900],[1260,1440,900],[1500,1440,900],[700,1440,900],[1260,1600,900],[1500,1600,900],[700,1600,900]]]
-}

{-> map solve' $ parse . drop 1 . lines $ example

[S 1,S 0,S 1,S 0,S 3,S 9]
-}

oneEach :: [[a]] -> [[a]]
oneEach [] = []
oneEach [as] = do
  a <- as
  [[a]]
oneEach (as:bss) = do
  bs <- oneEach bss
  a <- as
  [[a]++bs]

-- oneEach (a:as) = oneEach' [a] as
--   where
--     oneEach' :: [a] -> [a] -> [[a]]
--     oneEach' x xs = do
--       x' <- x
--       xs' <- oneEach' [x'] xs
--       xxs <- xs'
--       [[x'++xxs]]

{-> replicate 2 $ map (:[]) [1,2]


<interactive>:365:1: warning: [-Wtype-defaults]
    * Defaulting the following constraints to type `Integer'
        (Show a0)
          arising from a use of `System.IO.print' at <interactive>:365:1-29
        (Num a0) arising from a use of `it' at <interactive>:365:1-29
    * In a stmt of an interactive GHCi command: System.IO.print it
[[[1],[2]],[[1],[2]]]
-}

{-> oneEach [[1,2,3],[4,5,6],[7,8,9]]


<interactive>:1706:1: warning: [-Wtype-defaults]
    * Defaulting the following constraints to type `Integer'
        (Show a0)
          arising from a use of `System.IO.print' at <interactive>:1706:1-33
        (Num a0) arising from a use of `it' at <interactive>:1706:1-33
    * In a stmt of an interactive GHCi command: System.IO.print it
[[1,4,7],[2,4,7],[3,4,7],[1,5,7],[2,5,7],[3,5,7],[1,6,7],[2,6,7],[3,6,7],[1,4,8],[2,4,8],[3,4,8],[1,5,8],[2,5,8],[3,5,8],[1,6,8],[2,6,8],[3,6,8],[1,4,9],[2,4,9],[3,4,9],[1,5,9],[2,5,9],[3,5,9],[1,6,9],[2,6,9],[3,6,9]]
-}

write (S n) = toS $ " " ++ show n ++ "\n"

verify P{..} (S _) = Nothing
