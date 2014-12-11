{-# LANGUAGE PatternSynonyms #-}

module Data.YQL.Response
       ( Response(..)
       , pattern ResponseJSON
       , pattern ResponseByteString
       , object ) where

import Control.Lens

import Data.Aeson (Value)
import Data.ByteString.Lazy

data Response
  = Response
    { responseObject :: Either Value ByteString }

pattern ResponseJSON v = Response (Left v)
pattern ResponseByteString s = Response (Right s)

instance Show Response where
  show (Response (Left v)) = show v
  show (Response (Right s)) = show s

object :: Lens' Response (Either Value ByteString)
object = lens responseObject (\r o -> r { responseObject = o })
