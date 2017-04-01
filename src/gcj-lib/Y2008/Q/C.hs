module Y2008.Q.C where

import Protolude

import qualified Data.Text as Text (concat)
import Data.String

solve :: Text -> Text
solve =  Text.concat . map write . zip [1..] .  map solve' . parse . drop 1 . lines . toS

data P = P { f, bigR, t, r, g :: Double }

data S = S Double

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, S) -> Text

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads

parse (p:rest) =
  let (f':bigR':t':r':g':_) = map (read 0.0) $ words p
  in P f' bigR' t' r' g' : parse rest
parse _ = []

solve' P{..} | f >= (g/2) = S 1
solve' P{..} = S 0

write (i, S s) = toS $ "Case #" ++ show i ++ ": " ++ show s ++ "\n"
