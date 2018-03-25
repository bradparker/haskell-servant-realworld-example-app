module Data.Aeson.Extended
  ( module Data.Aeson
  , fieldLabelPrefixedBy
  ) where

import Control.Applicative ((<*>))
import Data.Aeson
import Data.Aeson.Casing (camelCase)
import Data.Aeson.Types (Options(..))
import Data.Function ((.))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.String (String)

fieldLabelPrefixedBy :: String -> Options
fieldLabelPrefixedBy prefix =
  defaultOptions
    {fieldLabelModifier = camelCase . (fromMaybe <*> stripPrefix prefix)}
