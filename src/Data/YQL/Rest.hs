{-# LANGUAGE OverloadedStrings,
             RankNTypes #-}

module Data.YQL.Rest
       ( Rest(..)
       , accept
       , contentType
       , header
       , headers
       , httpManager
       , httpRequest
       , timeout
       , url ) where

import Control.Lens

import Data.ByteString.Lazy
import Data.Map

import qualified Network.HTTP.Conduit as Conduit
import qualified Network.HTTP.Types.Header as HTTP

data Rest =
  Rest
  { restHttpManager :: Conduit.Manager
  , restHttpRequest :: Conduit.Request }

httpManager :: Lens' Rest Conduit.Manager
httpManager = lens restHttpManager (\rest m -> rest { restHttpManager = m })

httpRequest :: Lens' Rest Conduit.Request
httpRequest = lens restHttpRequest (\rest r -> rest { restHttpRequest = r })

requestHeaders :: Lens' Conduit.Request HTTP.RequestHeaders
requestHeaders =
  lens Conduit.requestHeaders (\r rhs -> r { Conduit.requestHeaders = rhs })

requestTimeout :: Lens' Conduit.Request (Maybe Int)
requestTimeout =
  lens
  Conduit.responseTimeout
  (\r t -> r { Conduit.responseTimeout = t })

requestUrl :: Lens' Conduit.Request ByteString
requestUrl =
  lens
  (fromStrict . Conduit.host)
  (\r u -> r { Conduit.host = toStrict u })

headers :: Lens' Rest HTTP.RequestHeaders
headers = httpRequest . requestHeaders

header :: HTTP.HeaderName -> Lens' Rest (Maybe ByteString)
header key =
  headers .
  lens fromList (const toList) .
  lens (fmap fromStrict) (const (fmap toStrict)) .
  at key

timeout :: Lens' Rest (Maybe Int)
timeout = httpRequest . requestTimeout

url :: Lens' Rest ByteString
url = httpRequest . requestUrl

accept :: Lens' Rest (Maybe ByteString)
accept = header "Accept"

contentType :: Lens' Rest (Maybe ByteString)
contentType = header "Content-Type"
