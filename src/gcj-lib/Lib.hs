module Lib where

import Protolude

import qualified Data.Map as Map

runner :: Text -> Maybe (Text -> Text)
runner = flip Map.lookup $ Map.fromList [("2008-PP-A", identity)]
