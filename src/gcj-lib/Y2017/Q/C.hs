module Y2017.Q.C where

import Protolude

import Data.String

import GCJ

parse :: [String] -> [P]
solve' :: P -> S
write :: S -> Text
verify :: P -> S -> Verification
solve :: Bool -> Text -> Text
solve = GCJ.solve (Parse parse) (Solve solve') (Write write) (Verify verify)

data P = P { n::Integer, k::Integer} deriving Show
data S = S Integer Integer deriving Show

parse (nk:rest) =
  let (n':k':_) = map (read 0) $ words nk
  in P n' k' : parse rest
parse _ = []

{-> parse . drop 1 . lines $ "5\n4 2\n5 2\n6 2\n1000 1000\n1000 1\n"

[P {n = 4, k = 2},P {n = 5, k = 2},P {n = 6, k = 2},P {n = 1000, k = 1000},P {n = 1000, k = 1}]
-}

solve' P{..} = uncurry S $ go' k []
  where go' :: Integer -> [(Integer, Integer)] -> (Integer, Integer)
        go' _ _ = (1, 2)
  -- let generation = (floor ((2 :: Double) `logBase` (fromIntegral k :: Double)) :: Int)
  --     rank = fromIntegral $ k - (2 ^ generation) :: Int
  --     maxLR = foldl (\s b -> if b then s `div` 2 else (s + 1) `div` 2) n $  map (\i -> testBit rank i) $ reverse [0..generation]
  --     minLR = foldl (\s b -> if b then s `div` 2 else (s - 1) `div` 2) n $  map (\i -> testBit rank i) $ reverse [0..generation]
  -- in S maxLR minLR

{-> map solve' $ parse . drop 1 . lines $ "5\n4 2\n5 2\n6 2\n1000 1000\n1000 1\n"

[S 1 2,S 1 2,S 1 2,S 1 2,S 1 2]
-}

write (S minLR maxLR) = toS $ " " ++ show minLR ++ " " ++ show maxLR ++ "\n"

verify P{..} _ = Nothing
