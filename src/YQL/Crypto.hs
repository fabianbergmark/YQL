module YQL.Crypto
       ( decodeBase64
       , encodeBase64
       , encodeMd5
       , encodeMd5Hex
       , encodeSha
       , uuid
       , encodeHmacSHA1
       , encodeHmacSHA256 ) where

import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1

import qualified Data.ByteString.Base64.Lazy as Base64
import Data.ByteString.Lazy
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.UUID as UUID (toLazyASCIIBytes)
import qualified Data.UUID.V4 as UUID

import Data.YQL (YQLM)

decodeBase64 :: ByteString -> Maybe ByteString
decodeBase64 s = either (const Nothing) Just (Base64.decode s)

encodeBase64 :: ByteString -> ByteString
encodeBase64 s = Base64.encode s

encodeHmacSHA1 :: ByteString -> ByteString -> ByteString
encodeHmacSHA1 secret plaintext =
  SHA.bytestringDigest $ SHA.hmacSha1 secret plaintext

encodeHmacSHA256 :: ByteString -> ByteString -> ByteString
encodeHmacSHA256 secret plaintext =
  SHA.bytestringDigest $ SHA.hmacSha256 secret plaintext

encodeMd5 :: ByteString -> ByteString
encodeMd5 message = fromStrict . MD5.hash . toStrict $ message

encodeMd5Hex :: ByteString -> ByteString
encodeMd5Hex = undefined

encodeSha :: ByteString -> ByteString
encodeSha message = fromStrict . SHA1.hash . toStrict $ message

uuid :: YQLM ByteString
uuid = liftIO $ UUID.nextRandom >>= return . UUID.toLazyASCIIBytes
