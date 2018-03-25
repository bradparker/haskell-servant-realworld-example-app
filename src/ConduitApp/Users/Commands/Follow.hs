module ConduitApp.Users.Commands.Follow
  ( execute
  , Error(..)
  ) where

import qualified ConduitApp.Users.Database as Database
import ConduitApp.Users.Database (User)
import ConduitApp.Web.Internal (Handle(..))
import Control.Applicative (pure)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Function (($))
import Data.Functor (void)
import Data.Pool (withResource)
import Data.Text (Text)
import Database.Beam (primaryKey)
import System.IO (IO)

data Error =
  NotFound

execute :: Handle -> User -> Text -> ExceptT Error IO User
execute Handle {connectionPool} follower username =
  maybeToExceptT NotFound $
  withResource connectionPool $ \conn -> do
    followee <- MaybeT $
      Database.findByUsername conn username
    void $ lift $
      Database.follow conn
        (primaryKey follower)
        (primaryKey followee)
    pure followee
