module Y2017.Q.B where

import Protolude

import Data.String

import GCJ

parse :: [String] -> [P]
solve' :: P -> S
write :: S -> Text
verify :: P -> S -> Verification
solve :: Bool -> Text -> Text
solve = GCJ.solve (Parse parse) (Solve solve') (Write write) (Verify verify)

data P = P Integer deriving Show
data S = S [Int] deriving Show

parse (n:rest) = P (read 0 n) : parse rest
parse _ = []

{-> parse . drop 1 . lines $ "4\n132\n1000\n7\n111111111111111110\n"

[P 132,P 1000,P 7,P 111111111111111110]
-}

digits :: Integer -> [Int]
digits n = map (read 0 . (:[])) $ show n :: [Int]

solve' (P n) =
  let tidy :: [Int] -> [Int]
      tidy [] = []
      tidy [n'] = [n']
      tidy (p:n':rest) | p > n' = p-1 : map (const 9) (n':rest)
      tidy (p:n':rest) = let (p':t') = tidy (n':rest)
                         in if p > p' then p-1 : map (const 9) (n':rest)
                            else p : p' : t'
  in S $ dropWhile (==0) $ tidy $ digits n

{-> map solve' $ parse . drop 1 . lines $ "4\n132\n1000\n7\n111111111111111110\n"

[S [1,2,9],S [9,9,9],S [7],S [9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9]]
-}

write (S ss) = toS $ " " ++ concatMap show ss ++ "\n"

verify (P n) (S ss) =
  let ss' = read 0 (concatMap show ss) in
  case head $ filter tidy $ reverse [ss' + 1..n] of
    Nothing -> if not $ tidy ss' then Just "" else Nothing
    Just x -> Just $ show x
  where tidy :: Integer -> Bool
        tidy i = fst $ foldl (\(b, prev) next -> (b && prev <= next, next)) (True, 0) (digits i)
