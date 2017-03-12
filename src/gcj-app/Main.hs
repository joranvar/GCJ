module Main where

import Protolude

import System.Remote.Monitoring (forkServer)

main :: IO ()
main = do
  _ <- forkServer "0.0.0.0" 8081
  putText "EKG running on http://localhost:8081"
  putText "hit any key to quit"
  args <- getArgs
  putText $ show args
