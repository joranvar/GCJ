module Lib where

import Protolude

import qualified Data.Map as Map

import qualified Y2017.R1A.A (solve)
import qualified Y2017.R1A.B (solve)
import qualified Y2017.R1A.C (solve)
import qualified Y2017.R1A.D (solve)

runner :: Text -> Maybe (Bool -> Text -> Text)
runner = flip Map.lookup $ Map.fromList
  [ ("2017-R1A-A", Y2017.R1A.A.solve)
  , ("2017-R1A-B", Y2017.R1A.B.solve)
  , ("2017-R1A-C", Y2017.R1A.C.solve)
  , ("2017-R1A-D", Y2017.R1A.D.solve) ]
