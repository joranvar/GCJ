module Y2017.Q.B where

import Protolude

import qualified Data.Text as Text (concat)
--import Data.List.Split
import Data.String

solve :: Text -> Text
solve =  Text.concat . map write . zip [1..] .  map solve' . parse . drop 1 . lines . toS

data P = P Integer
  deriving Show

data S = S [Int]

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

solve' (P n) =
  let digits = map (read 0 . (:[])) $ show n :: [Int]
      tidy :: [Int] -> [Int]
      tidy [] = []
      tidy [n'] = [n']
      tidy (p:n':rest) | p > n' = p-1 : map (const 9) (n':rest)
      tidy (p:n':rest) = let (p':t') = tidy (n':rest)
                         in if p' == 0 then p-1 : map (const 9) (n':rest)
                            else p : p' : t'
  in S $ dropWhile (==0) $ tidy digits

write (i, S ss) = toS $ "Case #" ++ show i ++ ": " ++ concatMap show ss ++ "\n"
