{-# LANGUAGE OverloadedStrings,
             RecordWildCards #-}

module YQL.Y
       ( decompress
       , deflate
       , inflate
       , rest ) where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib

import Control.Lens

import Data.ByteString.Lazy
import qualified Data.ByteString.Base64.Lazy as Base64

import Network.HTTP.Conduit (parseUrl)

import qualified Data.YQL as Data
import qualified Data.YQL.Rest as Data

decompress :: ByteString -> (Maybe ByteString)
decompress e = either (const Nothing) (Just . GZip.decompress) (Base64.decode e)

deflate :: ByteString -> Int -> ByteString
deflate string level =
  Base64.encode $
  Zlib.compressWith
  Zlib.defaultCompressParams {
    Zlib.compressLevel = Zlib.compressionLevel level }
  string

inflate :: ByteString -> Maybe ByteString
inflate e =
  case Base64.decode e of
   Right string ->
     Just $ Zlib.decompress string
   _ -> Nothing

rest :: String -> Data.YQLM ()
rest url = do
  req <- parseUrl url
  Data.rest . Data.httpRequest .= req
