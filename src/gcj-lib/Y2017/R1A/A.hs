module Y2017.R1A.A where

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
example = "3\n3 3\nG??\n?C?\n??J\n3 4\nCODE\n????\n?JAM\n2 2\nCA\nKE"

data P = P { cake::[[Char]] } deriving Show
data S = S { grid::[[Char]] } deriving Show

parse (rc:rest) =
  let (r:_) = map (read 0) $ words rc
      (cake', rest') = splitAt r rest
  in P cake' : parse rest'
parse _ = []

{-> parse . drop 1 . lines $ example

[P {cake = ["G??","?C?","??J"]},P {cake = ["CODE","????","?JAM"]},P {cake = ["CA","KE"]}]
-}

solve' P{..} =
  let hor :: String -> String
      hor ('?':rest) = let c = fromMaybe '?' (firstInitial rest) in c:(map (const c) $ takeWhile (=='?') rest) ++ (hor $ dropWhile (=='?') rest)
      hor (c:rest) = c:(map (const c) $ takeWhile (=='?') rest) ++ (hor $ dropWhile (=='?') rest)
      hor [] = []
      firstInitial rest = head $ dropWhile (=='?') rest
      ver :: [String] -> [String]
      ver (l:rest) | all (=='?') l = let cs = fromMaybe (map (const '?') l) (firstGood rest) in cs:(map (const cs) $ takeWhile (all (=='?')) rest) ++ (ver $ dropWhile (all (=='?')) rest)
      ver (cs:rest) = cs:(map (const cs) $ takeWhile (all (=='?')) rest) ++ (ver $ dropWhile (all (=='?')) rest)
      ver [] = []
      firstGood rest = head $ dropWhile (all (=='?')) rest
  in
  S $ ver $ map hor cake

{-> map solve' $ parse . drop 1 . lines $ example

[S {grid = ["GGG","CCC","JJJ"]},S {grid = ["CODE","CODE","JJAM"]},S {grid = ["CA","KE"]}]
-}

write S{..} = toS $ "\n" ++ concat (map (++ "\n") grid)

verify P{..} S{..} =
  if any (any (== '?')) grid then Just "Incomplete"
  else Nothing
