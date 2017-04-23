module Y2017.R1B.B where

import Protolude

import Data.String

import GCJ

parse :: [String] -> [P]
solve' :: P -> S
write :: S -> Text
verify :: P -> S -> Verification
solve :: Bool -> Text -> Text
solve = GCJ.solve (Parse parse) (Solve solve') (Write write) (Verify verify)

example :: String
example = "4\n6 2 0 2 0 2 0\n3 1 0 2 0 0 0\n6 2 0 1 1 2 0\n4 0 0 2 0 0 2\n"

data P = P { r,o,y,g,b,v::Int
           , n::Int  } deriving Show
data S = Impossible | S [Char] deriving (Show, Eq)

parse (unicorns:rest) =
  let (n:r:o:y:g:b:v:_) = map (read 0) $ words unicorns
  in P r o y g b v n : parse rest
parse _ = []

{-> parse . drop 1 . lines $ example

[P {r = 2, o = 0, y = 2, g = 0, b = 2, v = 0, n = 6},P {r = 1, o = 0, y = 2, g = 0, b = 0, v = 0, n = 3},P {r = 2, o = 0, y = 1, g = 1, b = 2, v = 0, n = 6},P {r = 0, o = 0, y = 2, g = 0, b = 0, v = 2, n = 4}]
-}

solve' p' =
  let
    firstBuild :: [([Char], P)] -> S
    firstBuild = fromMaybe Impossible . head . dropWhile (==Impossible) . map (uncurry build)
    makeBuild :: [Char] -> P -> Char -> ([Char], P)
    makeBuild sol p@P{..} 'R' = (('R':sol), p{r=r-1})
    makeBuild sol p@P{..} 'O' = (('O':sol), p{o=o-1})
    makeBuild sol p@P{..} 'Y' = (('Y':sol), p{y=y-1})
    makeBuild sol p@P{..} 'G' = (('G':sol), p{g=g-1})
    makeBuild sol p@P{..} 'B' = (('B':sol), p{b=b-1})
    makeBuild sol p@P{..} 'V' = (('V':sol), p{v=v-1})
    makeBuild _ _ _ = undefined

    build :: [Char] -> P -> S
    build sol P{..} | (length sol) == n && (head sol /= (head $ reverse sol)) = S sol
    build sol P{..} | (length sol) == n = Impossible
    build sol@('R':_) p@P{..} = firstBuild $ map snd $ filter ((>0) . fst) [(y, makeBuild sol p 'Y'), (g, makeBuild sol p 'G'), (b, makeBuild sol p 'B')]
    build sol@('O':_) p@P{..} = firstBuild $ map snd $ filter ((>0) . fst) [(b, makeBuild sol p 'B')]
    build sol@('Y':_) p@P{..} = firstBuild $ map snd $ filter ((>0) . fst) [(r, makeBuild sol p 'R'), (v, makeBuild sol p 'V'), (b, makeBuild sol p 'B')]
    build sol@('G':_) p@P{..} = firstBuild $ map snd $ filter ((>0) . fst) [(r, makeBuild sol p 'R')]
    build sol@('B':_) p@P{..} = firstBuild $ map snd $ filter ((>0) . fst) [(r, makeBuild sol p 'R'), (o, makeBuild sol p 'O'), (y, makeBuild sol p 'Y')]
    build sol@('V':_) p@P{..} = firstBuild $ map snd $ filter ((>0) . fst) [(y, makeBuild sol p 'Y')]
    build _ P{..} | (<2) $ length $ filter (>0) [r,b,y] = Impossible
    build _ P{..} | (n%2==1) && ((==2) $ length $ filter (>0) [r,b,y]) = Impossible
    build _ P{..} | ((==2) $ length $ filter (>0) [r,b,y]) && notEqual = Impossible
      where notEqual = let (a:c:_) = filter (>0) [r,b,y] in a /= c
    build sol p@P{..} = firstBuild $ take 1 $ map snd $ filter ((>0) . fst) [(r, makeBuild sol p 'R'), (o, makeBuild sol p 'O'), (y, makeBuild sol p 'Y')
                                                                            ,(g, makeBuild sol p 'G'), (b, makeBuild sol p 'B'), (v, makeBuild sol p 'V')]
 in build "" p'

{-> map solve' $ parse . drop 1 . lines $ example

[S "BYBRYR",Impossible,S "BYBRGR",S "VYVY"]
-}

write Impossible = " IMPOSSIBLE\n"
write (S sol) = toS $ " " ++ sol ++ "\n"

verify P{..} (S sol) = if length sol /= n then Just "Length mismatch!" else Nothing
verify P{..} Impossible = Nothing
