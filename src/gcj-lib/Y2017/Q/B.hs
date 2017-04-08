module Y2017.Q.B where

import Protolude

import qualified Data.Text as Text (concat)
--import Data.List.Split
import Data.String

solve :: Bool -> Text -> Text
solve blindly =  Text.concat . map write . zip [1..] .  map (\p -> let s = solve' p in (s, if blindly then (True, "") else verify p s)) . parse . drop 1 . lines . toS

data P = P Integer
  deriving Show

data S = S [Int]

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, (S, (Bool, String))) -> Text
verify :: P -> S -> (Bool, String)

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads

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
                         in if p' == 0 then p-1 : map (const 9) (n':rest)
                            else p : p' : t'
  in S $ dropWhile (==0) $ tidy $ digits n

write (i, (S ss, (True, _))) = toS $ "Case #" ++ show i ++ ": " ++ concatMap show ss ++ "\n"
write (i, (S ss, (False, err))) = toS $ "!" ++ err ++ "Case #" ++ show i ++ ": " ++ concatMap show ss ++ "\n"

verify (P n) (S ss) =
  let ss' = read 0 (concatMap show ss) in
  case head $ filter tidy $ reverse [ss' + 1..n] of
    Nothing -> (True, "")
    Just x -> (False, "(" ++ show x ++ ")")
  where tidy :: Integer -> Bool
        tidy i = fst $ foldl (\(b, prev) next -> (b && prev <= next, next)) (True, 0) (digits i)
