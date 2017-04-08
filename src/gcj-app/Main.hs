module Main where

import Protolude

import System.Remote.Monitoring (forkServer)

import Lib (runner)

main :: IO ()
main = do
  _ <- forkServer "0.0.0.0" 8081
  args <- getArgs
  case args of
    ["--verify", name] -> run False $ toS name
    [name] -> run True $ toS name
    _ -> return ()

run :: Bool -> Text -> IO ()

run blindly name = do
  interact $ fromMaybe identity $ runner name <*> Just blindly
