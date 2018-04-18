module Khichdi.Types where

import Data.Time.Clock (UTCTime)

{-
- Create a user
- Edit a user
-}
data User = User { uid :: Int
                 , name :: String
                 , ulocation :: [Location]
                 } deriving (Eq, Show)

{-
- Create a location
- Edit a Location
- Delete a location
-}
data Location = Location { lid :: Int
                         , address :: String
                         } deriving (Eq, Show)

{-
- Create an order
- Edit an order
-}
data Order = Order { oid :: Int
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
