module Y2017.Q.B where

import Protolude

import qualified Data.Text as Text (concat)
--import Data.List.Split
import Data.String

solve :: Text -> Text
solve =  Text.concat . map write . zip [1..] .  map solve' . parse . drop 1 . lines . toS

data P = P
  deriving Show

data S = S

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, S) -> Text

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads

parse _ = []

{-> parse . drop 1 . lines $ "2\n5\n3\n1 1 1\n2 1 0 2 0\n1 5 0\n1\n2\n1 1 0\n1 1 1\n"

[P {n = 5, ms = [[(1,True)],[(0,False),(0,False)],[(0,False)]]},P {n = 1, ms = [[(0,False)],[(1,True)]]}]
-}

solve' P = S

write (i, S) = toS $ "Case #" ++ show i ++ ":" ++ "\n"
