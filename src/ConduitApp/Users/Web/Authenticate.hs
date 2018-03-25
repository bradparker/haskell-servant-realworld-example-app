module ConduitApp.Users.Web.Authenticate
  ( Authenticate
  , handler
  ) where

import qualified ConduitApp.Users.Database as Database
import ConduitApp.Users.Web.Account (Account(..), fromUser)
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((.))
import Data.Functor ((<$>), fmap)
import Data.Pool (withResource)
import Servant (Handler, err401, errBody, throwError)
import Servant.API ((:>), JSON, Post, ReqBody)
import Data.Maybe (Maybe(Nothing, Just))

type Authenticate =
  "api" :>
  "users" :>
  "login" :>
  ReqBody '[JSON] (Namespace "user" Database.Credentials) :>
  Post '[JSON] (Namespace "user" Account)

handler ::
     Handle
  -> Namespace "user" Database.Credentials
  -> Handler (Namespace "user" Account)
handler Handle {connectionPool, authSecret} (Namespace params) = do
  found <-
    fmap (Namespace . fromUser authSecret) <$>
    liftIO (withResource connectionPool (`Database.findByCredentials` params))
  case found of
    Nothing -> throwError (err401 {errBody = "Incorrect login or password"})
    Just user -> pure user
