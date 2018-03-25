module ConduitApp.Users.Web.Follow
  ( Follow
  , handler
  ) where

import qualified ConduitApp.Users.Database as Database
import ConduitApp.Users.Database.User (User)
import ConduitApp.Users.Web.Profile (Profile(..), fromUser)
import ConduitApp.Web.Auth (RequireAuth)
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.Functor (void)
import Data.Pool (withResource)
import Data.Text (Text)
import Database.Beam (primaryKey)
import Servant (Handler, err404, throwError)
import Servant.API ((:>), Capture, JSON, Post)
import System.IO (IO)

type Follow =
  RequireAuth :>
  "api" :>
  "profiles" :>
  Capture "username" Text :>
  "follow" :>
  Post '[JSON] (Namespace "user" Profile)

handler :: Handle -> User -> Text -> Handler (Namespace "user" Profile)
handler handle user username = do
  result <- liftIO $ runExceptT $ follow handle user username
  case result of
    Left NotFound -> throwError err404
    Right followee -> pure . Namespace $ followee

data Error =
  NotFound

follow :: Handle -> User -> Text -> ExceptT Error IO Profile
follow Handle {connectionPool} follower username =
  maybeToExceptT NotFound $
  withResource connectionPool $ \conn -> do
    followee <- MaybeT $
      Database.findByUsername conn username
    void $ lift $
      Database.follow conn
        (primaryKey follower)
        (primaryKey followee)
    pure $ fromUser followee
