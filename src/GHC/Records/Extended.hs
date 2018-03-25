module GHC.Records.Extended () where

import GHC.Records (HasField(getField))
import GHC.OverloadedLabels (IsLabel(fromLabel))

instance HasField field r a => IsLabel field (r -> a) where
  fromLabel = getField @field
