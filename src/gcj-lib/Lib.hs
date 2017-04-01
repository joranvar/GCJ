module Lib where

import Protolude

import qualified Data.Map as Map

import qualified Y2008.Q.A (solve)
import qualified Y2008.Q.B (solve)
import qualified Y2008.Q.C (solve)

runner :: Text -> Maybe (Text -> Text)
runner = flip Map.lookup $ Map.fromList
  [ ("2008-Q-A", Y2008.Q.A.solve)
  , ("2008-Q-B", Y2008.Q.B.solve)
  , ("2008-Q-C", Y2008.Q.C.solve) ]
