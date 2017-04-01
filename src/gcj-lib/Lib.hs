module Lib where

import Protolude

import qualified Data.Map as Map

import qualified Y2008.Q.A (solve)

runner :: Text -> Maybe (Text -> Text)
runner = flip Map.lookup $ Map.fromList
  [ ("2008-Q-A", Y2008.Q.A.solve) ]
