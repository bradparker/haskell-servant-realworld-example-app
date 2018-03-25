module ConduitApp.Articles.Web.Feed (handler, Feed) where

import qualified ConduitApp.Articles.Database as Database
import ConduitApp.Articles.Web.Article (Article, fromDecorated)
import ConduitApp.Users.Database.User (User)
import ConduitApp.Web.Auth (RequireAuth)
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Monad.IO.Class (liftIO)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (map)
import Data.Maybe (Maybe, fromMaybe)
import Data.Pool (withResource)
import Prelude (Integer)
import Servant (Handler)
import Servant.API ((:>), Get, JSON, QueryParam)

type Feed =
  RequireAuth :>
  "api" :>
  "articles" :>
  "feed" :>
  QueryParam "limit" Integer :>
  QueryParam "offset" Integer :>
  Get '[JSON] (Namespace "articles" [Article])

handler ::
     Handle
  -> User
  -> Maybe Integer
  -> Maybe Integer
  -> Handler (Namespace "articles" [Article])
handler Handle {connectionPool} user limit offset =
  liftIO $
  withResource connectionPool $ \conn ->
    Namespace . map fromDecorated <$>
    Database.feed conn user (fromMaybe 20 limit) (fromMaybe 0 offset)
