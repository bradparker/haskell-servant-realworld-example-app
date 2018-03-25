module ConduitApp.Tags.Web
  ( tagAPIServer
  , TagAPI
  ) where

import qualified ConduitApp.Tags.Database as Database
import ConduitApp.Tags.Database.Tag (TagT(tagName))
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Monad.IO.Class (liftIO)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (map)
import Data.Pool (withResource)
import Data.Text (Text)
import Servant (Handler, Server)
import Servant.API ((:>), Get, JSON)

type QueryAll =
  "api" :>
  "tags" :>
  Get '[JSON] (Namespace "tags" [Text])

queryAll :: Handle -> Handler (Namespace "tags" [Text])
queryAll Handle {connectionPool} =
  liftIO $
  withResource connectionPool $
  (Namespace . map tagName <$>) . Database.query

type TagAPI = QueryAll

tagAPIServer :: Handle -> Server TagAPI
tagAPIServer = queryAll
