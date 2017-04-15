module Y2017.R1A.C where

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
example = "4\n11 5 16 5 0 0\n3 1 3 2 2 0\n3 1 3 2 1 0\n2 1 5 1 1 1\n"

data P = P { hd,ad,hk,ak,b,d::Int } deriving Show
data S = S | Impossible deriving Show

parse (p:rest) =
  let (hd':ad':hk':ak':b':d':_) = map (read 0) $ words p
  in P hd' ad' hk' ak' b' d' : parse rest
parse _ = []

{-> parse . drop 1 . lines $ example

[P {hd = 11, ad = 5, hk = 16, ak = 5, b = 0, d = 0},P {hd = 3, ad = 1, hk = 3, ak = 2, b = 2, d = 0},P {hd = 3, ad = 1, hk = 3, ak = 2, b = 1, d = 0},P {hd = 2, ad = 1, hk = 5, ak = 1, b = 1, d = 1}]
-}

solve' P{..} = S

{-> map solve' $ parse . drop 1 . lines $ example

[S,S,S,S]
-}

write S = toS $ " " ++ "\n"
write Impossible = " IMPOSSIBLE\n"

verify P{..} S = Nothing
verify P{..} Impossible = Nothing
