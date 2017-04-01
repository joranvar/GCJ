module Main where

import Protolude

import System.Remote.Monitoring (forkServer)

import Lib (runner)

main :: IO ()
main = do
  _ <- forkServer "0.0.0.0" 8081
  args <- getArgs
  let problem = maybe "2008-Q-A" toS $ head args
  interact $ fromMaybe identity $ runner problem
