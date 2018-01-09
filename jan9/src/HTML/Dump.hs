module HTML.Dump
  ( dump
  , X(..)
  ) where

import Network.Wreq (get, post)

-- Do this if you want to alias something you're importing...
-- import qualified Network.Wreq as W (get)
-- dump url = W.get url

dump url = get url

-- this `pratul` function isn't exported to the rest of the project because
-- in the module config right up top, we only specify "dump" function to be exported.
pratul = undefined

data X = This | That
