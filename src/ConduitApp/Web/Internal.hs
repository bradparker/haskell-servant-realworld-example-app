{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module ConduitApp.Web.Internal
  ( Handle(..)
  , new
  ) where

import ConduitApp.Database (openConduitDb)
import Control.Applicative ((<*>))
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Pool (Pool, createPool)
import Data.Text (pack)
import Database.PostgreSQL.Simple (Connection, close)
import System.Environment (getEnv)
import Web.JWT (Secret, secret)
import System.IO (IO)

data Handle = Handle
  { authSecret :: Secret
  , connectionPool :: Pool Connection
  }

createConnectionPool :: IO (Pool Connection)
createConnectionPool = createPool openConduitDb close 1 10 8

getAuthSecret :: IO Secret
getAuthSecret = secret . pack <$> getEnv "AUTH_SECRET"

new :: IO Handle
new = Handle <$> getAuthSecret <*> createConnectionPool
