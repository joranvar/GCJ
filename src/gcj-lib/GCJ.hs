{-# LANGUAGE ScopedTypeVariables #-}
module GCJ where

import Protolude
import Control.Parallel.Strategies
import Data.String (String, lines)
import qualified Data.Text as T (concat)

newtype Parse p = Parse { parser :: [String] -> [p] }
newtype Solve p s = Solve { solver :: p -> s }
type Verification = Maybe String
newtype Write s = Write { writer :: s -> Text }
newtype Verify p s = Verify { verifier :: p -> s -> Verification}

solve :: forall p s. Parse p -> Solve p s -> Write s -> Verify p s -> Bool -> Text -> Text
solve (Parse parse) (Solve solve') (Write write) (Verify verify) blindly =
  let writeWithVerification :: (Int, (s, Verification)) -> Text
      writeWithVerification (i, (s, Nothing)) = T.concat [toS $ "Case #" ++ show i ++ ":", write s]
      writeWithVerification (i, (s, Just err)) = T.concat [toS $"!(" ++ err ++ ")", writeWithVerification (i, (s, Nothing))]
  in
    T.concat . map writeWithVerification . zip [1..] .  parMap rpar (\p -> let s = solve' p in (s, if blindly then Nothing else verify p s)) . parse . drop 1 . lines . toS

read :: (Read a) => a -> String -> a
read d = maybe d fst . head . reads
