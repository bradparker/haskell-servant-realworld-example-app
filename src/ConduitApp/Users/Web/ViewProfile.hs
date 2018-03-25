module ConduitApp.Users.Web.ViewProfile
  ( handler
  , ViewProfile
  ) where

import qualified ConduitApp.Users.Database as Database
import ConduitApp.Users.Web.Profile (Profile, fromUser)
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Pool (withResource)
import Data.Text (Text)
import Servant (Handler, err404, throwError)
import Servant.API ((:>), Capture, Get, JSON)

type ViewProfile =
  "api" :>
  "profiles" :>
    Capture "username" Text :>
  Get '[JSON] (Namespace "profile" Profile)

handler :: Handle -> Text -> Handler (Namespace "profile" Profile)
handler Handle {connectionPool} username = do
  result <-
    liftIO $
    withResource connectionPool $ \conn ->
      Database.findByUsername conn username
  case result of
    Nothing -> throwError err404
    Just user -> pure $ Namespace $ fromUser user
