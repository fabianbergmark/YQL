module Data.YQL.Result
       ( Result(..)
       , response
       , headers
       , status
       , timeout
       , url ) where

import Control.Lens

import Data.ByteString

import qualified Network.HTTP.Types as HTTP

import Data.YQL.Response

data Result
  = Result
    { resultResponse :: Response
    , resultHeaders  :: HTTP.ResponseHeaders
    , resultStatus   :: HTTP.Status
    , resultTimeout  :: Bool
    , resultUrl      :: ByteString }

response :: Lens' Result Response
response = lens resultResponse (\res r -> res { resultResponse = r })

headers :: Lens' Result HTTP.ResponseHeaders
headers = lens resultHeaders (\res hs -> res { resultHeaders = hs })

status :: Lens' Result HTTP.Status
status = lens resultStatus (\res s -> res { resultStatus = s })

timeout :: Lens' Result Bool
timeout = lens resultTimeout (\res t -> res { resultTimeout = t })

url :: Lens' Result ByteString
url = lens resultUrl (\res u -> res { resultUrl = u })
