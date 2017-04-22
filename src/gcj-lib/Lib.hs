module Lib where

import Protolude

import qualified Data.Map as Map

import qualified Y2017.R1B.A (solve)
import qualified Y2017.R1B.B (solve)
import qualified Y2017.R1B.C (solve)
import qualified Y2017.R1B.D (solve)

runner :: Text -> Maybe (Bool -> Text -> Text)
runner = flip Map.lookup $ Map.fromList
  [ ("2017-R1B-A", Y2017.R1B.A.solve)
  , ("2017-R1B-B", Y2017.R1B.B.solve)
  , ("2017-R1B-C", Y2017.R1B.C.solve)
  , ("2017-R1B-D", Y2017.R1B.D.solve) ]
