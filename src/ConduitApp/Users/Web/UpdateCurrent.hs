module ConduitApp.Users.Web.UpdateCurrent
  ( UpdateCurrent
  , handler
  ) where

import ConduitApp.Users.Database.Attributes
  ( ValidationFailure
  , updateAttributes
  )
import qualified ConduitApp.Users.Database as Database
import ConduitApp.Users.Database.User (User)
import ConduitApp.Users.Web.Account (Account(..), fromUser)
import ConduitApp.Web.Auth (RequireAuth)
import ConduitApp.Web.Internal (Handle(..))
import System.IO (IO)
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, withExceptT)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Servant (Handler, err422, errBody, throwError)
import Servant.API ((:>), JSON, Put, ReqBody)
import Text.Show (show, Show)
import Data.Text (Text)
import Data.Aeson.Extended (FromJSON)
import Data.Maybe (Maybe(Nothing))
import GHC.Generics (Generic)
import Control.Monad.Trans.Class (lift)
import Data.Pool (withResource)

type UpdateCurrent  =
  RequireAuth :>
  "api" :>
  "user" :>
  ReqBody '[JSON] (Namespace "user" Params) :>
  Put '[ JSON] (Namespace "user" Account)

handler ::
     Handle
  -> User
  -> Namespace "user" Params
  -> Handler (Namespace "user" Account)
handler handle user (Namespace params) = do
  result <- liftIO $ runExceptT $ update handle user params
  case result of
    Left (FailedValidation errors) ->
      throwError err422 {errBody = pack (show errors)}
    Right updated -> pure . Namespace $ updated

newtype Error =
  FailedValidation [ValidationFailure]
  deriving (Show)

data Params = Params
  { email :: Maybe Text
  , username :: Maybe Text
  , bio :: Maybe Text
  , image :: Maybe (Maybe Text)
  } deriving (Show, Generic)

deriving instance FromJSON Params

update :: Handle -> User -> Params -> ExceptT Error IO Account
update Handle {connectionPool, authSecret} user Params {email , username , bio , image} =
  withResource connectionPool $ \conn -> do
    dbParams <-
      withExceptT FailedValidation $
      updateAttributes conn user Nothing email username bio image
    lift $ fromUser authSecret <$> Database.update conn user dbParams
