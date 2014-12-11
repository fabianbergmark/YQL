{-# LANGUAGE OverloadedStrings,
             RecordWildCards #-}

module YQL.Rest
       ( del
       , get
       , head
       , post
       , put ) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS
import Data.Monoid ((<>))

import Network.HTTP.Conduit
  (Request(..), Response(..), RequestBody(..),
   httpLbs)

import Prelude hiding (head)

import System.Log.Logger

import qualified Data.YQL as Data
import qualified Data.YQL.Response as Data.Response
import qualified Data.YQL.Rest as Data.Rest
import qualified Data.YQL.Result as Data.Result

del :: Data.YQLM Data.Result.Result
del = do
  request <- use $ Data.rest . Data.Rest.httpRequest
  httpRequest request { method = "DELETE" }

get :: Data.YQLM Data.Result.Result
get = do
  request <- use $ Data.rest . Data.Rest.httpRequest
  httpRequest request { method = "GET" }

head :: Data.YQLM Data.Result.Result
head = do
  request <- use $ Data.rest . Data.Rest.httpRequest
  httpRequest request { method = "HEAD" }

post :: LBS.ByteString -> Data.YQLM Data.Result.Result
post body = do
  request <- use $ Data.rest . Data.Rest.httpRequest
  httpRequest request { method = "POST"
                      , requestBody = RequestBodyLBS body }

put :: LBS.ByteString -> Data.YQLM Data.Result.Result
put body = do
  request <- use $ Data.rest . Data.Rest.httpRequest
  httpRequest request { method = "POST"
                      , requestBody = RequestBodyLBS body }

timeout :: Int -> Data.YQLM ()
timeout ms = do
  Data.rest . Data.Rest.timeout ?= (ms * 1000)

httpRequest :: Request -> Data.YQLM Data.Result.Result
httpRequest request = do
  manager <- use $ Data.rest . Data.Rest.httpManager
  lift $ do
    res <- httpLbs request manager
    let body = responseBody res
        status = responseStatus res
        headers = responseHeaders res
        verb = method request
        url = host request <> path request <> queryString request

    liftIO $
      infoM
      "YQL"
      ("HTTP " <> BS.toString verb <> " request to " <> BS.toString url)

    response <- case lookup "Content-Type" headers of
                 Just "application/json" -> do
                   case decode body of
                    Just v -> do
                      return $ Data.Response.ResponseJSON v
                    _ -> throwM Data.YQLExceptionJSONError
                 _ -> return $ Data.Response.ResponseByteString body
    return Data.Result.Result {
      resultResponse = response,
      resultHeaders = headers,
      resultStatus = status,
      resultTimeout = False,
      resultUrl = url }
