module ConduitApp.Articles.Web.QueryAll
  ( handler
  , QueryAll
  ) where

import qualified ConduitApp.Articles.Database as Database
import ConduitApp.Articles.Web.Article (Article, fromDecorated)
import ConduitApp.Users.Database.User (User)
import ConduitApp.Web.Auth (AcceptAuth)
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Monad.IO.Class (liftIO)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (map)
import Data.Maybe (Maybe, fromMaybe)
import Data.Pool (withResource)
import Data.Text (Text)
import Prelude (Integer)
import Servant (Handler)
import Servant.API ((:>), Get, JSON, QueryParam, QueryParams)

type QueryAll =
  AcceptAuth :>
  "api" :>
  "articles" :>
  QueryParam "limit" Integer :>
  QueryParam "offset" Integer :>
  QueryParams "tag" Text :>
  QueryParams "author" Text :>
  QueryParams "favorited" Text :>
  Get '[JSON] (Namespace "articles" [Article])

handler ::
     Handle
  -> Maybe User
  -> Maybe Integer
  -> Maybe Integer
  -> [Text]
  -> [Text]
  -> [Text]
  -> Handler (Namespace "articles" [Article])
handler Handle {connectionPool} user limit offset tags authors faved =
  liftIO $
  withResource connectionPool $ \conn ->
    Namespace . map fromDecorated <$>
    Database.query conn user params
  where
    params =
      Database.QueryParams
        (fromMaybe 20 limit)
        (fromMaybe 0 offset)
        tags
        authors
        faved
