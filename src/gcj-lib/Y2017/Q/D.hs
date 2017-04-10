module Y2017.Q.D where

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
example = "3\n2 0\n1 1\no 1 1\n3 4\n+ 2 3\n+ 2 1\nx 3 1\n+ 2 2"

type Model = Char
data Placement = Placement { m::Model, r, c::Int } deriving Show
data P = P { n::Int, models::[Placement]  } deriving Show
data S = S { y, z::Int, changedModels::[Placement] } deriving Show

parse (nm:rest) =
  let (n':m':_) = map (read 0) $ words nm
      (ms', rest') = splitAt m' rest
      parsePlacement pl = let ((m'':_):r':c':_) = words pl in Placement m'' (read 0 r') (read 0 c')
  in P n' (map parsePlacement ms') : parse rest'
parse _ = []

{-> parse . drop 1 . lines $ example

[P {n = 2, models = []},P {n = 1, models = [Placement {m = 'o', r = 1, c = 1}]},P {n = 3, models = [Placement {m = '+', r = 2, c = 3},Placement {m = '+', r = 2, c = 1},Placement {m = 'x', r = 3, c = 1},Placement {m = '+', r = 2, c = 2}]}]
-}

solve' P{..} = S 0 0 []

{-> map solve' $ parse . drop 1 . lines $ example

[S {y = 0, z = 0, changedModels = []},S {y = 0, z = 0, changedModels = []},S {y = 0, z = 0, changedModels = []}]
-}

write (S y z ms) = toS $ " " ++ show y ++ " " ++ show z ++ "\n" ++ concatMap ((++ "\n") . writePlacement) ms

writePlacement :: Placement -> String
writePlacement Placement{..} = writeModel m ++ " " ++ show r ++ " " ++ show c
writeModel :: Model -> String
writeModel = (:[])

verify P{..} S{..} = Nothing
