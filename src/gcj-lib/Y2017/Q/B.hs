module Y2017.Q.B where

import Protolude

import qualified Data.Text as Text (concat)
--import Data.List.Split
import Data.String

solve :: Text -> Text
solve =  Text.concat . map write . zip [1..] .  map solve' . parse . drop 1 . lines . toS

data P = P Integer
  deriving Show

data S = S Integer

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, S) -> Text

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads

parse (n:rest) = P (read 0 n) : parse rest
parse _ = []

{-> parse . drop 1 . lines $ "4\n132\n1000\n7\n111111111111111110\n"

[P 132,P 1000,P 7,P 111111111111111110]
-}

solve' (P _) = S 0

write (i, S s) = toS $ "Case #" ++ show i ++ ": " ++ show s ++ "\n"
