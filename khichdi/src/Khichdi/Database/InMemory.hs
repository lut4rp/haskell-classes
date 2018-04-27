module Khichdi.Database.InMemory where

import Khichdi.Types
import Data.List (find)

data Database = Database { users :: [User]
                         , locations :: [Location]
                         , orders :: [Order]
                         } deriving (Eq, Show)

initDb :: Database
initDb = Database [] [] []

getNextUserId :: Database -> UserId
getNextUserId db =
  let usrs = users db
  in
    case usrs of
      []    -> 0
      (x:_) -> 1 + uid x

addUser :: Database -> String -> Database
addUser db name =
  let nextId = getNextUserId db
      user   = User nextId name []
      newDb  = db { users = user : users db }
  in newDb

getNextLocationId :: Database -> LocationId
getNextLocationId db =
  let locns = locations db
  in
    case locns of
      [] -> 0
      (locn : _) -> 1 + lid locn

addLocationToUser :: Database -> String -> UserId -> Database
addLocationToUser db address userId =
  let maybeuser = getUserById db userId
      nextLocId = getNextLocationId db
      location  = Location nextLocId address
      updatedLocations = location : locations db
  in
    case maybeuser of
      Just user -> let updatedUser = user { ulocation = location : ulocation user }
                       updatedDb   = db { users = updatedUser : users db }
                   in undefined
      Nothing   -> error "This is not a valid user!"

getUserById :: Database -> UserId -> Maybe User
getUserById db userId = find (\u -> uid u == userId) (users db)

-- editUserName :: Database -> Int -> String -> Database
-- editUserName db userId newUserName =
--   let maybeuser = getUserById db userId
--   in  Just user =
--       Nothing   =
