module ConduitApp.Users.Database.Commands
  ( create
  , update
  , follow
  , unfollow
  ) where

import ConduitApp.Database
  ( ConduitDb(conduitFollows, conduitUsers)
  , conduitDb
  , insertOne
  )
import ConduitApp.Users.Database.Attributes (Attributes(..))
import ConduitApp.Users.Database.Follow (Follow, FollowT(..))
import qualified ConduitApp.Users.Database.User as User
import ConduitApp.Users.Database.User (User, UserId, UserT(..))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe, catMaybes, fromMaybe, listToMaybe)
import Database.Beam
  ( (&&.)
  , (<-.)
  , (==.)
  , default_
  , delete
  , insertExpressions
  , primaryKey
  , runDelete
  , val_
  )
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning(..))
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import System.IO (IO)

create :: Connection -> Attributes Identity -> IO User
create conn (Attributes password email username bio image) =
  insertOne
    conn
    (conduitUsers conduitDb)
    (insertExpressions
       [ User
           { _id = default_
           , password = val_ password
           , email = val_ email
           , bio = val_ bio
           , username = val_ username
           , image = val_ image
           }
       ])

update :: Connection -> User -> Attributes Maybe -> IO User
update conn currentUser (Attributes password email username bio image) =
  fromMaybe currentUser . listToMaybe <$>
  runBeamPostgres
    conn
    (runUpdateReturningList
       (conduitUsers conduitDb)
       (\user ->
          catMaybes
            [ (User.password user <-.) . val_ <$> password
            , (User.email user <-.) . val_ <$> email
            , (User.username user <-.) . val_ <$> username
            , (User.bio user <-.) . val_ <$> bio
            , (User.image user <-.) . val_ <$> image
            ])
       (\user -> primaryKey user ==. val_ (primaryKey currentUser)))

follow :: Connection -> UserId -> UserId -> IO Follow
follow conn followerId followeeId =
  insertOne
    conn
    (conduitFollows conduitDb)
    (insertExpressions
       [ Follow
           {followFollower = val_ followerId, followFollowee = val_ followeeId}
       ])

unfollow :: Connection -> UserId -> UserId -> IO ()
unfollow conn followerId followeeId =
  runBeamPostgres conn $
  runDelete $
    delete (conduitFollows conduitDb) $ \(Follow follower followee) ->
    follower ==. val_ followerId &&. followee ==. val_ followeeId
