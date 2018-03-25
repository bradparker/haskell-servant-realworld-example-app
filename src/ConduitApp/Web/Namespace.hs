{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module ConduitApp.Web.Namespace
  ( Namespace(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Data.Functor ((<$>))
import Data.Function (($))

newtype Namespace (ns :: Symbol) a =
  Namespace a

instance (KnownSymbol ns, ToJSON a) => ToJSON (Namespace ns a) where
  toJSON (Namespace a) = object [Text.pack (symbolVal (Proxy :: Proxy ns)) .= a]

instance (KnownSymbol ns, FromJSON a) => FromJSON (Namespace ns a) where
  parseJSON =
    withObject "Namespace" $ \v ->
      Namespace <$> v .: Text.pack (symbolVal (Proxy :: Proxy ns))
