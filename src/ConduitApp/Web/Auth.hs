{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module ConduitApp.Web.Auth
  ( AcceptAuth
  , RequireAuth
  , optionallyAuthorize
  , strictlyAuthorize
  ) where

import ConduitApp.Users.Database.User (User)
import qualified ConduitApp.Users.Database as User
import ConduitApp.Web.Internal (Handle(..))
import Control.Applicative (pure)
import Control.Error.Util (hoistMaybe)
import Control.Monad ((<=<), (=<<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Aeson (Value(String))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Pool (withResource)
import Data.Text (Text, stripPrefix)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai (Request, requestHeaders)
import Servant (AuthProtect, Handler, err403, throwError)
import Servant.Server.Experimental.Auth
  ( AuthHandler
  , AuthServerData
  , mkAuthHandler
  )
import Web.JWT (Secret, claims, decodeAndVerifySignature, unregisteredClaims)

type AcceptAuth = AuthProtect "JWTOptional"

type RequireAuth = AuthProtect "JWTRequired"

type instance AuthServerData AcceptAuth = Maybe User

type instance AuthServerData RequireAuth = User

usernameFromJWT :: Secret -> Text -> Maybe Text
usernameFromJWT authSecret =
  (\case
     String username -> Just username
     _ -> Nothing) <=<
  Map.lookup "username" . unregisteredClaims . claims <=<
  decodeAndVerifySignature authSecret

jwtFromRequest :: Request -> Maybe Text
jwtFromRequest req =
  stripPrefix "Token " =<<
  decodeUtf8 <$> List.lookup "Authorization" (requestHeaders req)

authHandler :: Handle -> Request -> Handler (Maybe User)
authHandler Handle {authSecret, connectionPool} req =
  liftIO $
  withResource connectionPool $ \conn ->
    runMaybeT $ do
      username <-
        hoistMaybe $ do
          jwt <- jwtFromRequest req
          usernameFromJWT authSecret jwt
      MaybeT $ User.findByUsername conn username

optionallyAuthorize :: Handle -> AuthHandler Request (Maybe User)
optionallyAuthorize handle = mkAuthHandler $ authHandler handle

strictlyAuthorize :: Handle -> AuthHandler Request User
strictlyAuthorize handle =
  mkAuthHandler $ \req -> do
    user <- authHandler handle req
    maybe (throwError err403) pure user
