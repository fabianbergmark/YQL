{-# LANGUAGE TemplateHaskell #-}

module Data.YQL.Y
       ( Y(..)
       , cache
       , context
       , crypto ) where

import Control.Lens (Lens', lens)

import Data.YQL.Cache (Cache)
import Data.YQL.Context (Context)
import Data.YQL.Crypto (Crypto)

data Y =
  Y
  { yCache      :: Cache
  , yContext    :: Context
  , yCrypto     :: Crypto }

cache :: Lens' Y Cache
cache = lens yCache (\y c -> y { yCache = c })

context :: Lens' Y Context
context = lens yContext (\y c -> y { yContext = c })

crypto :: Lens' Y Crypto
crypto = lens yCrypto (\y c -> y { yCrypto = c })
