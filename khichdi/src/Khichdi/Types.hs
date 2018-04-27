module Khichdi.Types where

import Data.Time.Clock (UTCTime)

{-
- Create a user
- Edit a user
-}
type UserId = Int
data User = User { uid :: UserId
                 , name :: String
                 , ulocation :: [Location]
                 } deriving (Eq, Show)

{-
- Create a location
- Edit a Location
- Delete a location
-}
type LocationId = Int
data Location = Location { lid :: LocationId
                         , address :: String
                         } deriving (Eq, Show)

{-
- Create an order
- Edit an order
-}
type OrderId = Int
data Order = Order { oid :: OrderId
                   , date :: UTCTime
                   , quantity :: Int
                   , user :: User
                   , olocation :: Location
                   , status :: OrderStatus
                   } deriving (Eq, Show)

data OrderStatus = OrderPlaced
                 | OrderCanceled
                 | OrderConfirmed
                 | OrderEnRoute
                 | OrderDelivered
                 | OrderPaid
                 deriving (Eq, Show)
