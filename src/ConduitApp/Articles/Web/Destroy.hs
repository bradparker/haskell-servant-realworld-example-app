module ConduitApp.Articles.Web.Destroy
  ( Destroy
  , handler
  ) where

import qualified ConduitApp.Articles.Database as Database
import ConduitApp.Articles.Web.Article (Article(..), fromDecorated)
import ConduitApp.Users.Database.User (User)
import ConduitApp.Web.Auth (RequireAuth)
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Monad.IO.Class (liftIO)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Pool (withResource)
import Data.Text (Text)
import Database.Beam (primaryKey)
import Servant (Handler, err404, throwError)
import Servant.API ((:>), Capture, Delete, JSON)

type Destroy =
  RequireAuth :>
  "api" :>
  "articles" :>
  Capture ":slug" Text :>
  Delete '[JSON] (Namespace "article" Article)

handler ::
     Handle
  -> User
  -> Text
  -> Handler (Namespace "article" Article)
handler Handle {connectionPool} user slug = do
  found <-
    liftIO $ withResource connectionPool $ \conn ->
      Database.findBySlug conn slug
  case found of
    Nothing -> throwError err404
    Just article ->
      liftIO $ withResource connectionPool $ \conn -> do
        Database.destroy conn (primaryKey article)
        Namespace . fromDecorated <$> Database.decorate conn user article
