module Y2017.Q.C where

import Protolude

import qualified Data.Text as Text (concat)
--import Data.List.Split
import Data.String

solve :: Bool -> Text -> Text
solve blindly =  Text.concat . map write . zip [1..] .  map (\p -> let s = solve' p in (s, if blindly then (True, "") else verify p s)) . parse . drop 1 . lines . toS

data P = P { n::Integer, k::Integer}
  deriving Show

data S = S Integer Integer

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, (S, (Bool, String))) -> Text
verify :: P -> S -> (Bool, String)

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads

parse (nk:rest) =
  let (n':k':_) = map (read 0) $ words nk
  in P n' k' : parse rest
parse _ = []

{-> parse . drop 1 . lines $ "5\n4 2\n5 2\n6 2\n1000 1000\n1000 1\n"

[P {n = 4, k = 2},P {n = 5, k = 2},P {n = 6, k = 2},P {n = 1000, k = 1000},P {n = 1000, k = 1}]
-}

solve' P{..} = uncurry S $ go' k []
  where go' :: Integer -> [(Integer, Integer)] -> (Integer, Integer)
        go' k _ = (1, 2)
  -- let generation = (floor ((2 :: Double) `logBase` (fromIntegral k :: Double)) :: Int)
  --     rank = fromIntegral $ k - (2 ^ generation) :: Int
  --     maxLR = foldl (\s b -> if b then s `div` 2 else (s + 1) `div` 2) n $  map (\i -> testBit rank i) $ reverse [0..generation]
  --     minLR = foldl (\s b -> if b then s `div` 2 else (s - 1) `div` 2) n $  map (\i -> testBit rank i) $ reverse [0..generation]
  -- in S maxLR minLR

write (i, (S minLR maxLR, v)) = toS $ writeVerification v ++ "Case #" ++ show i ++ ": " ++ show minLR ++ " " ++ show maxLR ++ "\n"

writeVerification :: (Bool, String) -> String
writeVerification (True, _) = ""
writeVerification (False, err) = "!(" ++ err ++ ")"

verify P{..} _ = (True, "")
