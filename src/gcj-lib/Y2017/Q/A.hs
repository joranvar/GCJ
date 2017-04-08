module Y2017.Q.A where

import Protolude

import qualified Data.Text as Text (concat)
--import Data.List.Split
import Data.String

solve :: Text -> Text
solve =  Text.concat . map write . zip [1..] .  map solve' . parse . drop 1 . lines . toS

data P = P { cs::[Bool], k::Int }
  deriving Show

data S = S Int | Impossible

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, S) -> Text

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads

parse (x:rest) =
  let (cs':k':_) = words x
  in P (map (\case '+' -> True ; _ -> False) cs') (read 0 k') : parse rest
parse _ = []

{-> parse . drop 1 . lines $ "3\n---+-++- 3\n+++++ 4\n-+-+- 4\n"

[P {cs = [False,False,False,True,False,True,True,False], k = 3},P {cs = [True,True,True,True,True], k = 4},P {cs = [False,True,False,True,False], k = 4}]
-}

solve' P{..} = S 0

write (i, S s) = toS $ "Case #" ++ show i ++ ": " ++ show s ++ "\n"
write (i, Impossible) = toS $ "Case #" ++ show i ++ ": IMPOSSIBLE\n"
