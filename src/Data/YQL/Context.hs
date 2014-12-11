module Data.YQL.Context
       ( Context(..)
       , host
       , table ) where

import Control.Lens (Lens', lens)

data Context =
  Context
  { contextHost  :: String
  , contextTable :: String }

host :: Lens' Context String
host = lens contextHost (\c h -> c { contextHost = h })

table :: Lens' Context String
table = lens contextTable (\c t -> c { contextTable = t })
