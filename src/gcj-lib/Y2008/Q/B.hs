{-# LANGUAGE RecordWildCards #-}

module Y2008.Q.B where

import Protolude

import qualified Data.Text as Text (concat)
import Data.List.Split (splitOneOf)
import Data.String

solve :: Text -> Text
solve =  Text.concat . map write . zip [1..] .  map solve' . parse . drop 1 . lines . toS

data Time = Time { hr, mn :: Int }
data Trip = Trip { dep, arr :: Time }
data P = P { t :: Int
           , nas, nbs :: [Trip]
           }

data S = S Int Int

parse :: [String] -> [P]
solve' :: P -> S
write :: (Int, S) -> Text

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads

parse (t':nanb:rest) =
  let (na:nb:_) = map (read 0) $ words nanb
      (nas', rest') = splitAt na rest
      (nbs', rest'') = splitAt nb rest'
  in P (maybe 0 fst $ head $ reads t') (map parseTrip nas') (map parseTrip nbs') : parse rest''
  where
    parseTrip :: String -> Trip
    parseTrip s = let [hrd,mnd,hra,mna] = map (read 0) . splitOneOf ": " $ s
      in Trip (Time hrd mnd) (Time hra mna)
parse _ = []

solve' P{..} = S (length nas - length (timelyArrivals (sort $ map ((+) t . time.arr) nbs) (sort $ map (time.dep) nas)))
                 (length nbs - length (timelyArrivals (sort $ map ((+) t . time.arr) nas) (sort $ map (time.dep) nbs)))
  where
    time :: Time -> Int
    time Time{..} = 60*hr + mn

    timelyArrivals :: [Int] -> [Int] -> [(Int, Int)]
    timelyArrivals [] _ = []
    timelyArrivals _ [] = []
    timelyArrivals (a:arr') (d:dep') | a <= d = (a, d) : timelyArrivals arr' dep'
    timelyArrivals (arr') (_:dep') = timelyArrivals arr' dep'

write (i, S a b) = toS $ "Case #" ++ show i ++ ": " ++ show a ++ " " ++ show b ++ "\n"
