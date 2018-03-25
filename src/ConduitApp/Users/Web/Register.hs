module ConduitApp.Users.Web.Register
  ( Register
  , handler
  ) where

import qualified ConduitApp.Users.Database as Database
import ConduitApp.Users.Database.Attributes
  ( ValidationFailure
  , insertAttributes
  )
import ConduitApp.Users.Web.Account (Account(..), fromUser)
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, withExceptT)
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing))
import Data.Pool (withResource)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (Handler, err422, errBody, throwError)
import Servant.API ((:>), JSON, PostCreated, ReqBody)
import System.IO (IO)
import Text.Show (Show, show)

type Register =
  "api" :>
  "users" :>
  ReqBody '[JSON] (Namespace "user" Params) :>
  PostCreated '[JSON] (Namespace "user" Account)

handler ::
     Handle -> Namespace "user" Params -> Handler (Namespace "user" Account)
handler handle (Namespace params) = do
  result <- liftIO $ runExceptT $ register handle params
  case result of
    Left (FailedValidation errors) ->
      throwError err422 {errBody = pack (show errors)}
    Right user -> pure . Namespace $ user

newtype Error =
  FailedValidation [ValidationFailure]
  deriving (Show)

data Params = Params
  { password :: Text
  , email :: Text
  , username :: Text
  } deriving (Generic, Show)

deriving instance FromJSON Params

register :: Handle -> Params -> ExceptT Error IO Account
register Handle {connectionPool, authSecret} Params {password, email, username} =
  withResource connectionPool $ \conn -> do
    attributes <-
      withExceptT FailedValidation $
      insertAttributes conn password email username "" Nothing
    lift $ fromUser authSecret <$> Database.create conn attributes
