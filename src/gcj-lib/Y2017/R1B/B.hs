module Y2017.R1B.B where

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
example = "4\n6 2 0 2 0 2 0\n3 1 0 2 0 0 0\n6 2 0 1 1 2 0\n4 0 0 2 0 0 2\n"

data P = P { r,o,y,g,b,v::Int
           , n::Int  } deriving Show
data S = Impossible | S [Char] deriving Show

parse (unicorns:rest) =
  let (n:r:o:y:g:b:v:_) = map (read 0) $ words unicorns
  in P r o y g b v n : parse rest
parse _ = []

{-> parse . drop 1 . lines $ example

[P {r = 2, o = 0, y = 2, g = 0, b = 2, v = 0, n = 6},P {r = 1, o = 0, y = 2, g = 0, b = 0, v = 0, n = 3},P {r = 2, o = 0, y = 1, g = 1, b = 2, v = 0, n = 6},P {r = 0, o = 0, y = 2, g = 0, b = 0, v = 2, n = 4}]
-}

solve' P{..} =
  Impossible

{-> map (oneEach . map snd . ingredients) $ parse . drop 1 . lines $ example

[[[900,660]],[[1500,809]],[[450,1100],[449,1100],[450,1101],[449,1101]],[[300,500]],[[11],[13],[17],[11],[16],[14],[12],[18]],[[1260,800,1700],[1500,800,1700],[700,800,1700],[1260,1440,1700],[1500,1440,1700],[700,1440,1700],[1260,1600,1700],[1500,1600,1700],[700,1600,1700],[1260,800,1620],[1500,800,1620],[700,800,1620],[1260,1440,1620],[1500,1440,1620],[700,1440,1620],[1260,1600,1620],[1500,1600,1620],[700,1600,1620],[1260,800,900],[1500,800,900],[700,800,900],[1260,1440,900],[1500,1440,900],[700,1440,900],[1260,1600,900],[1500,1600,900],[700,1600,900]]]
-}

{-> map solve' $ parse . drop 1 . lines $ example

[S 1,S 0,S 1,S 0,S 3,S 9]
-}

write Impossible = " IMPOSSIBLE\n"
write (S sol) = toS $ " " ++ show sol ++ "\n"

verify P{..} (S sol) = if length sol /= n then Just "Length mismatch!" else Nothing
verify P{..} Impossible = Nothing
