module ConduitApp.Users.Web.Unfollow
  ( Unfollow
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
import Servant.API ((:>), Capture, Delete, JSON)
import System.IO (IO)

type Unfollow =
  RequireAuth :>
  "api" :>
  "profiles" :>
  Capture "username" Text :>
  "follow" :>
  Delete '[JSON] (Namespace "user" Profile)

handler :: Handle -> User -> Text -> Handler (Namespace "user" Profile)
handler handle user username = do
  result <- liftIO $ runExceptT $ unfollow handle user username
  case result of
    Left NotFound -> throwError err404
    Right unfollowee -> pure . Namespace $ unfollowee

data Error =
  NotFound

unfollow :: Handle -> User -> Text -> ExceptT Error IO Profile
unfollow Handle {connectionPool} unfollower username =
  maybeToExceptT NotFound $
  withResource connectionPool $ \conn -> do
    unfollowee <- MaybeT $
      Database.findByUsername conn username
    void $ lift $
      Database.unfollow conn
        (primaryKey unfollower)
        (primaryKey unfollowee)
    pure $ fromUser unfollowee
