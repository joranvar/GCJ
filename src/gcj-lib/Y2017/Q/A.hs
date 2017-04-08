module Y2017.Q.A where

import Protolude

import qualified Data.Text as Text (concat)
--import Data.List.Split
import Data.String

solve :: Bool -> Text -> Text
solve blindly =  Text.concat . map write . zip [1..] .  map (\p -> let s = solve' p in (s, if blindly then (True, "") else verify p s)) . parse . drop 1 . lines . toS

data P = P { cs::[Bool], k::Int }
  deriving Show

data S = S Int | Impossible

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, (S, (Bool, String))) -> Text
verify :: P -> S -> (Bool, String)

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads

parse (x:rest) =
  let (cs':k':_) = words x
  in P (map (\case '+' -> True ; _ -> False) cs') (read 0 k') : parse rest
parse _ = []

{-> parse . drop 1 . lines $ "3\n---+-++- 3\n+++++ 4\n-+-+- 4\n"

[P {cs = [False,False,False,True,False,True,True,False], k = 3},P {cs = [True,True,True,True,True], k = 4},P {cs = [False,True,False,True,False], k = 4}]
-}

solve' P{..} | all identity cs = S 0
solve' P{..} = Impossible

write (i, (S s, v)) = toS $ writeVerification v ++ "Case #" ++ show i ++ ": " ++ show s ++ "\n"
write (i, (Impossible, v)) = toS $ writeVerification v ++ "Case #" ++ show i ++ ": IMPOSSIBLE\n"
writeVerification :: (Bool, String) -> String
writeVerification (True, _) = ""
writeVerification (False, err) = "!(" ++ err ++ ")"

verify P{..} _ = (False, "")
