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

solve' P{..} = S []

{-> map solve' $ parse . drop 1 . lines $ example

[]
-}

write S{..} = toS $ " " ++ "\n"

verify P{..} S{..} = Nothing
