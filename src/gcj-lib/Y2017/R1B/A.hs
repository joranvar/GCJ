module Y2017.R1B.A where

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
example = "3\n2525 1\n2400 5\n300 2\n120 60\n60 90\n100 2\n80 100\n70 10\n"

data Horse = Horse { position,speed::Int } deriving Show
data P = P { d::Int, hs::[Horse] } deriving Show
data S = S { y::Double } deriving Show

parse (dn:rest) =
  let
    (d':n:_) = map (read 0) $ words dn
    (hs', rest') = splitAt n rest
    hs'' = map parseHorse hs'
    parseHorse ks = let (k:s:_) = map (read 0) $ words ks in Horse k s
    in P d' hs'' : parse rest'
parse _ = []

{-> parse . drop 1 . lines $ example

[P {d = 2525, hs = [Horse {position = 2400, speed = 5}]},P {d = 300, hs = [Horse {position = 120, speed = 60},Horse {position = 60, speed = 90}]},P {d = 100, hs = [Horse {position = 80, speed = 100},Horse {position = 70, speed = 10}]}]
-}

solve' P{..} = S 0

{-> map solve' $ parse . drop 1 . lines $ example

[S {grid = ["GGG","CCC","JJJ"]},S {grid = ["CODE","CODE","JJAM"]},S {grid = ["CA","KE"]}]
-}

write S{..} = toS $ " " ++ show y

verify P{..} S{..} = Nothing
