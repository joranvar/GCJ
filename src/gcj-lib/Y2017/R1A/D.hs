module Y2017.R1A.D where

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
example = ""

data P = P deriving Show
data S = S deriving Show

parse (_:rest) = P : parse rest
parse _ = []

{-> parse . drop 1 . lines $ example

[]
-}

solve' P = S

{-> map solve' $ parse . drop 1 . lines $ example

[]
-}

write S = toS $ " " ++ "\n"

verify P S = Nothing
