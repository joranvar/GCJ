module Y2017.R1B.C where

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
example = "3\n3 1\n2 3\n2 4\n4 4\n-1 1 -1\n-1 -1 1\n-1 -1 -1\n1 3\n4 1\n13 10\n1 1000\n10 8\n5 5\n-1 1 -1 -1\n-1 -1 1 -1\n-1 -1 -1 10\n-1 -1 -1 -1\n1 4\n4 3\n30 60\n10 1000\n12 5\n20 1\n-1 10 -1 31\n10 -1 10 -1\n-1 -1 -1 10\n15 6 -1 -1\n2 4\n3 1\n3 2\n"

data Horse = Horse { dist,speed::Int } deriving Show
data Route = Route Int Int deriving Show
data P = P { hs::[Horse]
           , dsts::[[Maybe Int]]
           , routes::[Route] } deriving Show
data S = S [Int] deriving Show

parse (nq:rest) =
  let (n:q:_) = map (read 0) $ words nq
      (hs', rest') = splitAt n rest
      (dsts', rest'') = splitAt n rest'
      (routes', rest''') = splitAt q rest''
      parseHorse h = let (e:s:_) = map (read 0) $ words h in Horse e s
      parseDists ds = (map.map) ((\d -> if d == -1 then Nothing else Just d) . (read (0::Int))) $ map words ds
      parseRoute r = let (u:v:_) = map (read 0) $ words r in Route u v
  in P (map parseHorse hs') (parseDists dsts') (map parseRoute routes') : parse rest'''
parse _ = []

{-> parse . drop 1 . lines $ example

[P {hs = [Horse {dist = 2, speed = 3},Horse {dist = 2, speed = 4},Horse {dist = 4, speed = 4}], dsts = [[Nothing,Just 1,Nothing],[Nothing,Nothing,Just 1],[Nothing,Nothing,Nothing]], routes = [Route 1 3]},P {hs = [Horse {dist = 13, speed = 10},Horse {dist = 1, speed = 1000},Horse {dist = 10, speed = 8},Horse {dist = 5, speed = 5}], dsts = [[Nothing,Just 1,Nothing,Nothing],[Nothing,Nothing,Just 1,Nothing],[Nothing,Nothing,Nothing,Just 10],[Nothing,Nothing,Nothing,Nothing]], routes = [Route 1 4]},P {hs = [Horse {dist = 30, speed = 60},Horse {dist = 10, speed = 1000},Horse {dist = 12, speed = 5},Horse {dist = 20, speed = 1}], dsts = [[Nothing,Just 10,Nothing,Just 31],[Just 10,Nothing,Just 10,Nothing],[Nothing,Nothing,Nothing,Just 10],[Just 15,Just 6,Nothing,Nothing]], routes = [Route 2 4,Route 3 1,Route 3 2]}]
-}

solve' P{..} = S []

{-> map solve' $ parse . drop 1 . lines $ example

[S,S,S,S]
-}

write (S ys) = toS $ concatMap (\y -> " " ++ show y) ys ++ "\n"

verify P{..} (S ys) =
  if length ys /= length routes then Just "Route count mismatch"
  else Nothing
