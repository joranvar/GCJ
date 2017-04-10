module Y2017.Q.A where

import Protolude

import Data.String

import GCJ

parse :: [String] -> [P]
solve' :: P -> S
write :: S -> Text
verify :: P -> S -> Verification
solve :: Bool -> Text -> Text
solve = GCJ.solve (Parse parse) (Solve solve') (Write write) (Verify verify)

data P = P { cs::[Bool], k::Int } deriving Show
data S = S Int | Impossible deriving Show

parse (x:rest) =
  let (cs':k':_) = words x
  in P (map (\case '+' -> True ; _ -> False) cs') (read 0 k') : parse rest
parse _ = []

{-> parse . drop 1 . lines $ "3\n---+-++- 3\n+++++ 4\n-+-+- 4\n"

[P {cs = [False,False,False,True,False,True,True,False], k = 3},P {cs = [True,True,True,True,True], k = 4},P {cs = [False,True,False,True,False], k = 4}]
-}

solve' P{..} = go' (S 0) $ dropWhile identity cs
  where go' :: S -> [Bool] -> S
        go' (Impossible) _ = Impossible
        go' (S i) cs' | k >= length cs' && all identity cs' = S i
        go' (S i) cs' | k == length cs' && all not cs' = S (i + 1)
        go' _ cs' | k >= length cs' = Impossible
        go' (S i) cs' = let (_:k'd, rest) = splitAt k cs' in go' (S (i+1)) $ dropWhile identity $ (map not k'd) ++ rest

{-> map solve' $ parse . drop 1 . lines $ "3\n---+-++- 3\n+++++ 4\n-+-+- 4\n"

[S 3,S 0,Impossible]
-}

write (S s) = toS $ " " ++ show s ++ "\n"
write Impossible = " IMPOSSIBLE\n"

verify P{..} _ = Nothing
