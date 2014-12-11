{-# LANGUAGE PatternSynonyms,
             RecordWildCards #-}

module Language.JavaScript.Host.YQL.Rest where

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad (forM)
import Control.Monad.Trans.Class (lift)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy.UTF8 as LBS
import Data.Default (def)
import qualified Data.HashMap.Strict as HashMap (toList)
import Data.String
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as Vector (toList)

import qualified Network.HTTP.Types as HTTP

import Data.OpenDataTable
import Data.YQL (YQLM)
import qualified Data.YQL as Data
import Data.YQL.Response (Response(..),
                          pattern ResponseByteString,
                          pattern ResponseJSON)
import qualified Data.YQL.Response as Data.Response
import Data.YQL.Result (Result(..))
import qualified Data.YQL.Result as Data.Result
import qualified Data.YQL.Rest as Data.Rest

import Language.JavaScript.Host
import Language.JavaScript.Interpret
import Language.JavaScript.SubType

import qualified YQL.Rest as YQL

import qualified Data.Map as Map (empty, fromList)

request :: OpenDataTable -> Select -> JavaScriptT YQLM Object
request OpenDataTable {..} s = do
  url <- lift . lift . use $ Data.rest . Data.Rest.url

  op <- use objectPrototypeObject
  fp <- use functionPrototypeObject

  yqlRestId <- createNextInternalId
  let yqlRestObj = Object yqlRestId

  yqlRestAcceptId <- createNextInternalId
  let yqlRestAcceptObj = Object yqlRestAcceptId
      yqlRestAcceptObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just (yqlRestAcceptCallImpl yqlRestObj),
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestAcceptObj ?= yqlRestAcceptObjInt

  yqlRestContentTypeId <- createNextInternalId
  let yqlRestContentTypeObj = Object yqlRestContentTypeId
      yqlRestContentTypeObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just (yqlRestContentTypeCallImpl yqlRestObj),
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestContentTypeObj ?= yqlRestContentTypeObjInt

  yqlRestDecompressId <- createNextInternalId
  let yqlRestDecompressObj = Object yqlRestDecompressId
      yqlRestDecompressObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just (yqlRestDecompressCallImpl yqlRestObj),
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestDecompressObj ?= yqlRestDecompressObjInt

  yqlRestDelId <- createNextInternalId
  let yqlRestDelObj = Object yqlRestDelId
      yqlRestDelObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestDelCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestDelObj ?= yqlRestDelObjInt

  yqlRestFallbackCharsetId <- createNextInternalId
  let yqlRestFallbackCharsetObj = Object yqlRestFallbackCharsetId
      yqlRestFallbackCharsetObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestFallbackCharsetCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestFallbackCharsetObj ?= yqlRestFallbackCharsetObjInt

  yqlRestFilterCharsId <- createNextInternalId
  let yqlRestFilterCharsObj = Object yqlRestFilterCharsId
      yqlRestFilterCharsObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestFilterCharsCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestFilterCharsObj ?= yqlRestFilterCharsObjInt

  yqlRestForceCharsetId <- createNextInternalId
  let yqlRestForceCharsetObj = Object yqlRestForceCharsetId
      yqlRestForceCharsetObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestForceCharsetCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestForceCharsetObj ?= yqlRestForceCharsetObjInt

  yqlRestGetId <- createNextInternalId
  let yqlRestGetObj = Object yqlRestGetId
      yqlRestGetObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestGetCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestGetObj ?= yqlRestGetObjInt

  yqlRestHeadId <- createNextInternalId
  let yqlRestHeadObj = Object yqlRestHeadId
      yqlRestHeadObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestHeadCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestHeadObj ?= yqlRestHeadObjInt

  yqlRestHeadersId <- createNextInternalId
  let yqlRestHeadersObj = Object yqlRestHeadersId
      yqlRestHeadersObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist op),
        objectInternalClass             = "Object",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Nothing,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestHeadersObj ?= yqlRestHeadersObjInt

  yqlRestJsonCompatId <- createNextInternalId
  let yqlRestJsonCompatObj = Object yqlRestJsonCompatId
      yqlRestJsonCompatObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestJsonCompatCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestJsonCompatObj ?= yqlRestJsonCompatObjInt

  yqlRestPathId <- createNextInternalId
  let yqlRestPathObj = Object yqlRestPathId
      yqlRestPathObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestPathCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestPathObj ?= yqlRestPathObjInt

  yqlRestPostId <- createNextInternalId
  let yqlRestPostObj = Object yqlRestPostId
      yqlRestPostObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestPostCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestPostObj ?= yqlRestPostObjInt

  yqlRestPutId <- createNextInternalId
  let yqlRestPutObj = Object yqlRestPutId
      yqlRestPutObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestPutCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestPutObj ?= yqlRestPutObjInt

  yqlRestQueryId <- createNextInternalId
  let yqlRestQueryObj = Object yqlRestQueryId
      yqlRestQueryObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just (yqlRestQueryCallImpl yqlRestObj),
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestQueryObj ?= yqlRestQueryObjInt

  yqlRestQueryParamsId <- createNextInternalId
  let yqlRestQueryParamsObj = Object yqlRestQueryParamsId
      yqlRestQueryParamsObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestQueryParamsCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestQueryParamsObj ?= yqlRestQueryParamsObjInt

  yqlRestTimeoutId <- createNextInternalId
  let yqlRestTimeoutObj = Object yqlRestTimeoutId
      yqlRestTimeoutObjInt = ObjectInternal {
        objectInternalProperties        = Map.empty,
        objectInternalPrototype         = const $ return (JSExist fp),
        objectInternalClass             = "Function",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Just yqlRestTimeoutCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestTimeoutObj ?= yqlRestTimeoutObjInt

  let yqlRestObjInt = ObjectInternal {
        objectInternalProperties        =
           Map.fromList
           [ ("accept", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestAcceptObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("contentType", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestContentTypeObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("decompress", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestDecompressObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("del", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestDelObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("fallbackCharset", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestFallbackCharsetObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("filterChars", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestFilterCharsObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("forceCharset", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestForceCharsetObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("get", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestGetObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("head", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestHeadObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("headers", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestHeadersObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("jsonCompat", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestJsonCompatObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("path", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestPathObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("post", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestHeadersObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("put", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestPutObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("query", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestQueryObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("queryParams", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestQueryParamsObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("timeout", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlRestTimeoutObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("url", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj (LBS.toString url),
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True }) ],
        objectInternalPrototype         = const $ return (JSExist op),
        objectInternalClass             = "Object",
        objectInternalExtensible        = const $ return True,
        objectInternalGet               = getImpl,
        objectInternalGetOwnProperty    = getOwnPropertyImpl,
        objectInternalGetProperty       = getPropertyImpl,
        objectInternalPut               = putImpl,
        objectInternalCanPut            = canPutImpl,
        objectInternalHasProperty       = hasPropertyImpl,
        objectInternalDelete            = deleteImpl,
        objectInternalDefaultValue      = defaultValueImpl,
        objectInternalDefineOwnProperty = defineOwnPropertyImpl,
        objectInternalPrimitiveValue    = Nothing,
        objectInternalConstruct         = Nothing,
        objectInternalCall              = Nothing,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlRestObj ?= yqlRestObjInt

  defineGlobalProperty
    "request"
    (PropertyData DataDescriptor {
        dataDescriptorValue          = inj yqlRestObj,
        dataDescriptorWritable       = True,
        dataDescriptorEnumerable     = False,
        dataDescriptorConfigurable   = True })

  return yqlRestObj

yqlRestAcceptCallImpl :: Object -> InternalCallType YQLM
yqlRestAcceptCallImpl restObj f this (List args) = do
  let a = case args of
           (ValueString s:_) -> s
           _ -> ""
  lift . lift $ Data.rest . Data.Rest.accept ?= fromString a
  return (inj restObj)

yqlRestContentTypeCallImpl :: Object -> InternalCallType YQLM
yqlRestContentTypeCallImpl restObj f this (List args) = do
  let a = case args of
           (ValueString s:_) -> s
           _ -> ""
  lift . lift $ Data.rest . Data.Rest.contentType ?= fromString a
  return (inj restObj)

yqlRestDecompressCallImpl :: Object -> InternalCallType YQLM
yqlRestDecompressCallImpl restObj f this (List args) = undefined

yqlRestDelCallImpl :: InternalCallType YQLM
yqlRestDelCallImpl _ _ _ = wrappedHttpCall YQL.del

yqlRestFallbackCharsetCallImpl :: InternalCallType YQLM
yqlRestFallbackCharsetCallImpl = undefined

yqlRestFilterCharsCallImpl :: InternalCallType YQLM
yqlRestFilterCharsCallImpl = undefined

yqlRestForceCharsetCallImpl :: InternalCallType YQLM
yqlRestForceCharsetCallImpl = undefined

yqlRestGetCallImpl :: InternalCallType YQLM
yqlRestGetCallImpl _ _ _ = wrappedHttpCall YQL.get

yqlRestHeadCallImpl :: InternalCallType YQLM
yqlRestHeadCallImpl _ _ _ = wrappedHttpCall YQL.head

yqlRestJsonCompatCallImpl :: InternalCallType YQLM
yqlRestJsonCompatCallImpl = undefined

yqlRestPathCallImpl :: InternalCallType YQLM
yqlRestPathCallImpl = undefined

yqlRestPostCallImpl :: InternalCallType YQLM
yqlRestPostCallImpl _ _ (List args) = do
  case args of
   (ValueString body:_) -> do
     wrappedHttpCall $ YQL.post (fromString body)
   _ -> newTypeErrorObject Nothing >>= jsThrow

yqlRestPutCallImpl :: InternalCallType YQLM
yqlRestPutCallImpl _ _ (List args) = do
  case args of
   (ValueString body:_) -> do
     wrappedHttpCall $ YQL.post (fromString body)
   _ -> newTypeErrorObject Nothing >>= jsThrow

yqlRestQueryCallImpl :: Object -> InternalCallType YQLM
yqlRestQueryCallImpl restObj f this (List args) = do
  case args of
   (ValueString key:ValueString value:_) -> undefined
   _ -> return ()
  return (inj restObj)

yqlRestQueryParamsCallImpl :: InternalCallType YQLM
yqlRestQueryParamsCallImpl = undefined

yqlRestTimeoutCallImpl :: InternalCallType YQLM
yqlRestTimeoutCallImpl f this (List args) = do
  case args of
   (ValueNumber n:_) -> lift . lift $ do
     Data.rest . Data.Rest.timeout ?= round n
   _ -> return ()
  return (inj Undefined)

wrappedHttpCall :: YQLM Result -> JavaScriptT YQLM CallValue
wrappedHttpCall call = do
  op <- use objectPrototypeObject
  Result {..} <- lift . lift $ call
  let headersObj = undefined :: Object

  o <- newObjectObject Nothing

  defineOwnProperty o "headers" def {
    propertyDescriptorValue        = Just (inj headersObj),
    propertyDescriptorWritable     = Just True,
    propertyDescriptorEnumerable   = Just True,
    propertyDescriptorConfigurable = Just True } False

  defineOwnProperty o "status" def {
    propertyDescriptorValue        =
       Just (inj (Number . fromIntegral . HTTP.statusCode $ resultStatus)),
    propertyDescriptorWritable     = Just True,
    propertyDescriptorEnumerable   = Just True,
    propertyDescriptorConfigurable = Just True } False

  defineOwnProperty o "timeout" def {
    propertyDescriptorValue        = Just (inj resultTimeout),
    propertyDescriptorWritable     = Just True,
    propertyDescriptorEnumerable   = Just True,
    propertyDescriptorConfigurable = Just True } False

  defineOwnProperty o "timeout" def {
    propertyDescriptorValue        = Just (inj (BS.toString resultUrl)),
    propertyDescriptorWritable     = Just True,
    propertyDescriptorEnumerable   = Just True,
    propertyDescriptorConfigurable = Just True } False

  case resultResponse of
   ResponseJSON j -> do
     v <- jsonToValue j
     defineOwnProperty o "response" def {
       propertyDescriptorValue        = Just v,
       propertyDescriptorWritable     = Just True,
       propertyDescriptorEnumerable   = Just True,
       propertyDescriptorConfigurable = Just True } False

   ResponseByteString s -> do

     defineOwnProperty o "response" def {
       propertyDescriptorValue        = Just (inj (LBS.toString s)),
       propertyDescriptorWritable     = Just True,
       propertyDescriptorEnumerable   = Just True,
       propertyDescriptorConfigurable = Just True } False

  return (inj o)

jsonToValue :: (Functor m, Monad m) => Aeson.Value -> JavaScriptT m Value
jsonToValue (Aeson.Object jo) = do
  o <- newObjectObject Nothing
  forM (HashMap.toList jo) $ \(k, j) -> do
    v <- jsonToValue j
    let desc = def {
          propertyDescriptorValue        = Just v,
          propertyDescriptorWritable     = Just True,
          propertyDescriptorEnumerable   = Just True,
          propertyDescriptorConfigurable = Just True }
    defineOwnProperty o (Text.unpack k) desc False
  return (inj o)

jsonToValue (Aeson.Array ja) = do
  vs <- forM (Vector.toList ja) jsonToValue
  a <- newArrayObject vs
  return (inj a)

jsonToValue (Aeson.String s) = return . inj $ (Text.unpack s)
jsonToValue (Aeson.Number n) =
  return . inj . Number . fromRational . toRational $ n
jsonToValue (Aeson.Bool b) = return (inj b)
jsonToValue (Aeson.Null) = return (inj Null)
