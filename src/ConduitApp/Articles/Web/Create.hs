module ConduitApp.Articles.Web.Create
  ( handler
  , Create
  ) where

import qualified ConduitApp.Articles.Database as Database
import ConduitApp.Articles.Database.Attributes
  ( ValidationFailure
  , insertAttributes
  )
import ConduitApp.Articles.Web.Article (Article, fromDecorated)
import ConduitApp.Users.Database.User (User)
import ConduitApp.Web.Auth (RequireAuth)
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
import Data.Pool (withResource)
import Data.Text (Text)
import Database.Beam (primaryKey)
import GHC.Generics (Generic)
import Servant (Handler, err422, errBody, throwError)
import Servant.API ((:>), JSON, PostCreated, ReqBody)
import System.IO (IO)
import Text.Show (show)
import Data.Set (Set)

data Params = Params
  { title :: Text
  , description :: Text
  , body :: Text
  , tagList :: Set Text
  }

deriving instance Generic Params
deriving instance FromJSON Params

type Create =
  RequireAuth :>
  "api" :>
  "articles" :>
  ReqBody '[JSON] (Namespace "article" Params) :>
  PostCreated '[JSON] (Namespace "article" Article)

newtype Error =
  FailedValidation [ValidationFailure]

handler ::
     Handle
  -> User
  -> Namespace "article" Params
  -> Handler (Namespace "article" Article)
handler handle user (Namespace params) = do
  result <- liftIO $ runExceptT $ create handle user params
  case result of
    Left (FailedValidation errors) -> throwError err422 {errBody = pack (show errors)}
    Right article -> pure . Namespace $ article

create :: Handle -> User -> Params -> ExceptT Error IO Article
create Handle {connectionPool} user params =
  withResource connectionPool $ \conn -> do
    attributes <-
      withExceptT FailedValidation $
      insertAttributes conn (title params) (description params) (body params)
    lift $ do
      article <- Database.create conn (primaryKey user) attributes
      Database.assignTags
        conn
        (primaryKey article)
        (tagList params)
      fromDecorated <$> Database.decorate conn user article
