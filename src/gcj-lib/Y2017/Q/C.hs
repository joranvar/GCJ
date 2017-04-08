module Y2017.Q.C where

import Protolude

import qualified Data.Text as Text (concat)
--import Data.List.Split
import Data.String

solve :: Bool -> Text -> Text
solve _ =  Text.concat . map write . zip [1..] .  map solve' . parse . drop 1 . lines . toS

data P = P { n::Integer, k::Integer}
  deriving Show

data S = S Integer Integer

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, S) -> Text

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads

parse (nk:rest) =
  let (n':k':_) = map (read 0) $ words nk
  in P n' k' : parse rest
parse _ = []

{-> parse . drop 1 . lines $ "5\n4 2\n5 2\n6 2\n1000 1000\n1000 1\n"

[P {n = 4, k = 2},P {n = 5, k = 2},P {n = 6, k = 2},P {n = 1000, k = 1000},P {n = 1000, k = 1}]
-}

solve' P{..} = S 0 0

write (i, S minLR maxLR) = toS $ "Case #" ++ show i ++ ": " ++ show minLR ++ " " ++ show maxLR ++ "\n"
