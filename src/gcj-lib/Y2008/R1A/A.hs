module Y2008.R1A.A where

import Protolude

import qualified Data.Text as Text (concat)
import Data.String

solve :: Text -> Text
solve =  Text.concat . map write . zip [1..] .  map solve' . parse . drop 1 . lines . toS

data P = P { xs, ys :: [Integer] }

data S = S Integer

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, S) -> Text

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads

parse (_:xs':ys':rest) = P (map (read 0) $ words xs') (map (read 0) $ words ys') : parse rest
parse _ = []

solve' P{..} = S $ sum $ zipWith (*) (sort xs) (reverse $ sort ys)

write (i, S s) = toS $ "Case #" ++ show i ++ ": " ++ show s ++ "\n"
