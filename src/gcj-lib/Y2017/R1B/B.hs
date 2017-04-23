module Y2017.R1B.B where

import Protolude

import qualified Data.Map as Map (adjust, elems, fromList, filter, filterWithKey, toList)
import Data.String

import GCJ

parse :: [String] -> [P]
solve' :: P -> S
write :: S -> Text
verify :: P -> S -> Verification
solve :: Bool -> Text -> Text
solve = GCJ.solve (Parse parse) (Solve solve') (Write write) (Verify verify)

example :: String
example = "4\n6 2 0 2 0 2 0\n3 1 0 2 0 0 0\n6 2 0 1 1 2 0\n4 0 0 2 0 0 2\n"

data Unicorn = R | O | Y | G | B | V deriving (Show, Eq, Ord)
data P = P { us::Map Unicorn Int
           , n::Int  } deriving Show
data S = Impossible | S [Unicorn] deriving (Show, Eq)

normalize :: Map Unicorn Int -> Map Unicorn Int
normalize = Map.filter (>0)

parse (unicorns:rest) =
  let (n:r:o:y:g:b:v:_) = map (read 0) $ words unicorns
      us' = normalize $ Map.fromList [(R, r), (O, o), (Y, y), (G, g), (B, b), (V, v)]
  in P us' n : parse rest
parse _ = []

{-> parse . drop 1 . lines $ example

[P {us = fromList [(R,2),(Y,2),(B,2)], n = 6},P {us = fromList [(R,1),(Y,2)], n = 3},P {us = fromList [(R,2),(Y,1),(G,1),(B,2)], n = 6},P {us = fromList [(Y,2),(V,2)], n = 4}]
-}

solve' P{..} =
  let
    makeBuild :: [Unicorn] -> [Unicorn] -> Map Unicorn Int -> S
    makeBuild set sol us' = case head
                                 $ dropWhile (==Impossible)
                                 $ map (\(u, _) -> build (u:sol) (normalize $ Map.adjust (subtract 1) u us'))
                                 $ sortOn (negate . snd)
                                 $ Map.toList
                                 $ Map.filterWithKey (\k _ -> k `elem` set) us' of
      Nothing -> Impossible
      Just s -> s

    build :: [Unicorn] -> Map Unicorn Int -> S
    build sol _ | (length sol) == n && (head sol /= (head $ reverse sol)) = S sol
    build sol _ | (length sol) == n = Impossible
    build sol@(R:_) us' = makeBuild [Y,G,B] sol us'
    build sol@(O:_) us' = makeBuild [B] sol us'
    build sol@(Y:_) us' = makeBuild [R,V,B] sol us'
    build sol@(G:_) us' = makeBuild [R] sol us'
    build sol@(B:_) us' = makeBuild [R,O,Y] sol us'
    build sol@(V:_) us' = makeBuild [Y] sol us'
    build _ us' | (<2) $ length us' = Impossible
    build _ us' | (n%2==1) && ((==2) $ length us') = Impossible
    build _ us' | ((==2) $ length us') && notEqual = Impossible
      where notEqual = let (a:c:_) = Map.elems us' in a /= c
    build sol us' = makeBuild [R,O,Y,G,B,V] sol us'
 in build [] us

{-> map solve' $ parse . drop 1 . lines $ example

[S [B,Y,R,B,Y,R],Impossible,Impossible,S [V,Y,V,Y]]
-}

write Impossible = " IMPOSSIBLE\n"
write (S sol) = toS $ " " ++ concatMap show sol ++ "\n"

verify P{..} (S sol) = if length sol /= n then Just "Length mismatch!" else Nothing
verify P{..} Impossible = Nothing
