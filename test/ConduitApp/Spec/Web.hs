module ConduitApp.Spec.Web
  ( withHandle
  , runHandlerOptimistically
  ) where

import ConduitApp.Spec.Database (withConnection)
import ConduitApp.Web.Internal (Handle(..))
import Control.Applicative (pure)
import Control.Exception (throw)
import Data.Either (Either(Left, Right))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection)
import Servant (Handler, runHandler)
import System.IO (IO)
import Web.JWT (secret)

runHandlerOptimistically :: Handler a -> IO a
runHandlerOptimistically handler = do
  result <- runHandler handler
  case result of
    Left err -> throw err
    Right value -> pure value

createFakeConnectionPool :: Connection -> IO (Pool Connection)
createFakeConnectionPool conn =
  createPool (pure conn) (const (pure ())) 1 30 1

newFakeHandle :: Connection -> IO Handle
newFakeHandle conn = Handle (secret "foobar") <$> createFakeConnectionPool conn

withHandle :: (Handle -> IO a) -> IO a
withHandle action =
  withConnection $ \conn -> do
    handle <- newFakeHandle conn
    action handle
