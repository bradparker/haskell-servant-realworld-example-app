module ConduitApp.Users.Web
  ( UsersAPI
  , usersAPIServer
  ) where

import ConduitApp.Users.Web.Authenticate (Authenticate)
import qualified ConduitApp.Users.Web.Authenticate as Authenticate
import ConduitApp.Users.Web.Follow (Follow)
import qualified ConduitApp.Users.Web.Follow as Follow
import ConduitApp.Users.Web.Unfollow (Unfollow)
import qualified ConduitApp.Users.Web.Unfollow as Unfollow
import ConduitApp.Users.Web.GetCurrent (GetCurrent)
import qualified ConduitApp.Users.Web.GetCurrent as GetCurrent
import ConduitApp.Users.Web.Register (Register)
import qualified ConduitApp.Users.Web.Register as Register
import ConduitApp.Users.Web.UpdateCurrent (UpdateCurrent)
import qualified ConduitApp.Users.Web.UpdateCurrent as UpdateCurrent
import ConduitApp.Users.Web.ViewProfile (ViewProfile)
import qualified ConduitApp.Users.Web.ViewProfile as ViewProfile
import ConduitApp.Web.Internal (Handle)
import Servant (Server)
import Servant.API ((:<|>)((:<|>)))

type UsersAPI =
  Register :<|>
  Authenticate :<|>
  GetCurrent :<|>
  UpdateCurrent :<|>
  Follow :<|>
  Unfollow :<|>
  ViewProfile

usersAPIServer :: Handle -> Server UsersAPI
usersAPIServer handle =
  Register.handler handle :<|>
  Authenticate.handler handle :<|>
  GetCurrent.handler handle :<|>
  UpdateCurrent.handler handle :<|>
  Follow.handler handle :<|>
  Unfollow.handler handle :<|>
  ViewProfile.handler handle
