module Khichdi.Database.InMemory where

import Khichdi.Types

data Database = Database { users :: [User]
                         , locations :: [Location]
                         , orders :: [Order]
                         } deriving (Eq, Show)

initDb :: Database
initDb = Database [] [] []

getNextUserId :: Database -> Int
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
