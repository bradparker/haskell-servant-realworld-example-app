module ConduitApp.Users.Web.GetCurrent
  ( GetCurrent
  , handler
  ) where

import ConduitApp.Users.Database.User (User)
import ConduitApp.Users.Web.Account (Account(..), fromUser)
import ConduitApp.Web.Auth (RequireAuth)
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Applicative (pure)
import Data.Function ((.))
import Servant (Handler)
import Servant.API ((:>), Get, JSON)

type GetCurrent =
  RequireAuth :>
  "api" :>
  "user" :>
  Get '[JSON] (Namespace "user" Account)

handler :: Handle -> User -> Handler (Namespace "user" Account)
handler Handle {authSecret} = pure . Namespace . fromUser authSecret
