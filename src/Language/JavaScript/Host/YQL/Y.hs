{-# LANGUAGE PatternSynonyms #-}

module Language.JavaScript.Host.YQL.Y where

import Control.Lens ((?=), use)
import Control.Monad.Trans.Class (lift)

import qualified Data.ByteString.Lazy.UTF8 as LBS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map (empty, fromList)
import Data.String (fromString)

import Data.OpenDataTable
import Data.YQL

import Language.JavaScript.Host
import Language.JavaScript.Interpret
import Language.JavaScript.SubType

import qualified YQL.Y as Y
import qualified YQL.Crypto as Crypto

y :: OpenDataTable -> Select -> Object -> JavaScriptT YQLM ()
y ot s restObj = do
  op <- use objectPrototypeObject
  fp <- use functionPrototypeObject

  yqlYContextId <- createNextInternalId
  let yqlYContextObj = Object yqlYContextId
      yqlYContextObjInt = ObjectInternal {
        objectInternalProperties        =
           Map.fromList
           [ ("host", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = r.r.r.r.inj $ "",
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("table", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = r.r.r.r.inj $ "",
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

  mInternalObject yqlYContextObj ?= yqlYContextObjInt

  yqlYCryptoDecodeBase64Id <- createNextInternalId
  let yqlYCryptoDecodeBase64Obj = Object yqlYCryptoDecodeBase64Id
      yqlYCryptoDecodeBase64ObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYCryptoDecodeBase64CallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYCryptoDecodeBase64Obj ?= yqlYCryptoDecodeBase64ObjInt

  yqlYCryptoEncodeBase64Id <- createNextInternalId
  let yqlYCryptoEncodeBase64Obj = Object yqlYCryptoEncodeBase64Id
      yqlYCryptoEncodeBase64ObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYCryptoEncodeBase64CallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYCryptoEncodeBase64Obj ?= yqlYCryptoEncodeBase64ObjInt

  yqlYCryptoEncodeHmacSHA1Id <- createNextInternalId
  let yqlYCryptoEncodeHmacSHA1Obj = Object yqlYCryptoEncodeHmacSHA1Id
      yqlYCryptoEncodeHmacSHA1ObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYCryptoEncodeHmacSHA1CallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYCryptoEncodeHmacSHA1Obj ?= yqlYCryptoEncodeHmacSHA1ObjInt

  yqlYCryptoEncodeHmacSHA256Id <- createNextInternalId
  let yqlYCryptoEncodeHmacSHA256Obj = Object yqlYCryptoEncodeHmacSHA256Id
      yqlYCryptoEncodeHmacSHA256ObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYCryptoEncodeHmacSHA256CallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYCryptoEncodeHmacSHA256Obj ?= yqlYCryptoEncodeHmacSHA256ObjInt

  yqlYCryptoEncodeMd5Id <- createNextInternalId
  let yqlYCryptoEncodeMd5Obj = Object yqlYCryptoEncodeMd5Id
      yqlYCryptoEncodeMd5ObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYCryptoEncodeMd5CallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYCryptoEncodeMd5Obj ?= yqlYCryptoEncodeMd5ObjInt

  yqlYCryptoEncodeMd5HexId <- createNextInternalId
  let yqlYCryptoEncodeMd5HexObj = Object yqlYCryptoEncodeMd5HexId
      yqlYCryptoEncodeMd5HexObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYCryptoEncodeMd5HexCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYCryptoEncodeMd5HexObj ?= yqlYCryptoEncodeMd5HexObjInt

  yqlYCryptoEncodeShaId <- createNextInternalId
  let yqlYCryptoEncodeShaObj = Object yqlYCryptoEncodeShaId
      yqlYCryptoEncodeShaObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYCryptoEncodeShaCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYCryptoEncodeShaObj ?= yqlYCryptoEncodeShaObjInt

  yqlYCryptoUUIDId <- createNextInternalId
  let yqlYCryptoUUIDObj = Object yqlYCryptoUUIDId
      yqlYCryptoUUIDObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYCryptoUUIDCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYCryptoUUIDObj ?= yqlYCryptoUUIDObjInt

  yqlYCryptoId <- createNextInternalId
  let yqlYCryptoObj = Object yqlYCryptoId
      yqlYCryptoObjInt = ObjectInternal {
        objectInternalProperties        =
           Map.fromList
           [ ("decodeBase64", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYCryptoDecodeBase64Obj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("encodeBase64", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYCryptoEncodeBase64Obj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("encodeHmacSHA1", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYCryptoEncodeHmacSHA1Obj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("encodeHmacSHA256", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYCryptoEncodeHmacSHA256Obj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("encodeMd5", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYCryptoEncodeMd5Obj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("encodeMd5Hex", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYCryptoEncodeMd5HexObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("encodeSha", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYCryptoEncodeShaObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("uuid", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYCryptoUUIDObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })],
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

  mInternalObject yqlYCryptoObj ?= yqlYCryptoObjInt

  yqlYDateGetOffsetFromEpochInMillisId <- createNextInternalId
  let yqlYDateGetOffsetFromEpochInMillisObj =
        Object yqlYDateGetOffsetFromEpochInMillisId
      yqlYDateGetOffsetFromEpochInMillisObjInt = ObjectInternal {
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
        objectInternalCall              =
          Just yqlYDateGetOffsetFromEpochInMillisCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYDateGetOffsetFromEpochInMillisObj ?=
    yqlYDateGetOffsetFromEpochInMillisObjInt

  yqlYDateId <- createNextInternalId
  let yqlYDateObj = Object yqlYDateId
      yqlYDateObjInt = ObjectInternal {
        objectInternalProperties        =
           Map.fromList
           [ ("getOffsetFromEpochInMillis", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYDateGetOffsetFromEpochInMillisObj,
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

  mInternalObject yqlYDateObj ?= yqlYDateObjInt

  yqlYDecompressId <- createNextInternalId
  let yqlYDecompressObj = Object yqlYDecompressId
      yqlYDecompressObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYDecompressCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYDecompressObj ?= yqlYDecompressObjInt

  yqlYDeflateId <- createNextInternalId
  let yqlYDeflateObj = Object yqlYDeflateId
      yqlYDeflateObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYDeflateCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYDeflateObj ?= yqlYDeflateObjInt

  yqlYExitId <- createNextInternalId
  let yqlYExitObj = Object yqlYExitId
      yqlYExitObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYExitCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYExitObj ?= yqlYExitObjInt

  yqlYIncludeId <- createNextInternalId
  let yqlYIncludeObj = Object yqlYIncludeId
      yqlYIncludeObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYIncludeCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYIncludeObj ?= yqlYIncludeObjInt

  yqlYInflateId <- createNextInternalId
  let yqlYInflateObj = Object yqlYInflateId
      yqlYInflateObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYInflateCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYInflateObj ?= yqlYInflateObjInt

  yqlYJsToStringId <- createNextInternalId
  let yqlYJsToStringObj = Object yqlYJsToStringId
      yqlYJsToStringObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYJsToStringCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYJsToStringObj ?= yqlYJsToStringObjInt

  yqlYRestId <- createNextInternalId
  let yqlYRestObj = Object yqlYRestId
      yqlYRestObjInt = ObjectInternal {
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
        objectInternalCall              = Just (yqlYRestCallImpl restObj),
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYRestObj ?= yqlYRestObjInt

  yqlYSyncId <- createNextInternalId
  let yqlYSyncObj = Object yqlYSyncId
      yqlYSyncObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYSyncCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYSyncObj ?= yqlYSyncObjInt

  yqlYTidyId <- createNextInternalId
  let yqlYTidyObj = Object yqlYTidyId
      yqlYTidyObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYTidyCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYTidyObj ?= yqlYTidyObjInt

  yqlYXparseJsonId <- createNextInternalId
  let yqlYXparseJsonObj = Object yqlYXparseJsonId
      yqlYXparseJsonObjInt = ObjectInternal {
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
        objectInternalCall              = Just yqlYXparseJsonCallImpl,
        objectInternalHasInstance       = Nothing,
        objectInternalScope             = Nothing,
        objectInternalFormalParameters  = Nothing,
        objectInternalCode              = Nothing,
        objectInternalTargetFunction    = Nothing,
        objectInternalBoundThis         = Nothing,
        objectInternalBoundArguments    = Nothing,
        objectInternalMatch             = Nothing,
        objectInternalParameterMap      = Nothing }

  mInternalObject yqlYXparseJsonObj ?= yqlYXparseJsonObjInt

  yqlYId <- createNextInternalId
  let yqlYObj = Object yqlYId
      yqlYObjInt = ObjectInternal {
        objectInternalProperties        =
           Map.fromList
           [ ("context", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYContextObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("crypto", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYCryptoObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("date", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYDateObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("decompress", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYDecompressObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("deflate", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYDeflateObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("exit", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYExitObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("include", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYIncludeObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("inflate", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYInflateObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("jsToString", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYJsToStringObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("rest", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYRestObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("sync", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYSyncObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("tidy", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYTidyObj,
                 dataDescriptorWritable       = True,
                 dataDescriptorEnumerable     = False,
                 dataDescriptorConfigurable   = True })
           , ("xparseJson", PropertyData $ DataDescriptor {
                 dataDescriptorValue          = inj yqlYXparseJsonObj,
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

  mInternalObject yqlYObj ?= yqlYObjInt

  defineGlobalProperty
    "y"
    (PropertyData DataDescriptor {
        dataDescriptorValue          = inj yqlYObj,
        dataDescriptorWritable       = True,
        dataDescriptorEnumerable     = False,
        dataDescriptorConfigurable   = True })

yqlYCryptoDecodeBase64CallImpl :: InternalCallType YQLM
yqlYCryptoDecodeBase64CallImpl _ _ (List args) = do
  case args of
   (ValueString s:_) -> do
     case Crypto.decodeBase64 (fromString s) of
      Just d -> return . r.r.r.r.r.inj . LBS.toString $ d
      _ -> jsThrow . r.r.r.r.inj $ "Invalid Base64 encoding"
   _ -> newTypeErrorObject Nothing >>= jsThrow . inj

yqlYCryptoEncodeBase64CallImpl :: InternalCallType YQLM
yqlYCryptoEncodeBase64CallImpl  _ _ (List args) = do
  case args of
   (ValueString s:_) -> do
     return . r.r.r.r.r.inj . LBS.toString . Crypto.encodeBase64 . fromString $ s
   _ -> newTypeErrorObject Nothing >>= jsThrow . inj

yqlYCryptoEncodeHmacSHA1CallImpl :: InternalCallType YQLM
yqlYCryptoEncodeHmacSHA1CallImpl _ _ (List args) = do
  case args of
   (ValueString secret:ValueString plaintext:_) -> do
     let hex = Crypto.encodeHmacSHA1 (fromString secret) (fromString plaintext)
     return . r.r.r.r.r.inj . LBS.toString $ hex
   _ -> newTypeErrorObject Nothing >>= jsThrow . inj

yqlYCryptoEncodeHmacSHA256CallImpl :: InternalCallType YQLM
yqlYCryptoEncodeHmacSHA256CallImpl _ _ (List args) = do
  case args of
   (ValueString secret:ValueString plaintext:_) -> do
     let hex = Crypto.encodeHmacSHA256 (fromString secret) (fromString plaintext)
     return . r.r.r.r.r.inj . LBS.toString $ hex
   _ -> newTypeErrorObject Nothing >>= jsThrow . inj

yqlYCryptoEncodeMd5CallImpl :: InternalCallType YQLM
yqlYCryptoEncodeMd5CallImpl _ _ (List args) = do
  case args of
   (ValueString message:_) -> do
     let hex = Crypto.encodeMd5 . fromString $ message
     return . r.r.r.r.r.inj . LBS.toString $ hex
   _ -> newTypeErrorObject Nothing >>= jsThrow . inj

yqlYCryptoEncodeMd5HexCallImpl :: InternalCallType YQLM
yqlYCryptoEncodeMd5HexCallImpl = undefined

yqlYCryptoEncodeShaCallImpl :: InternalCallType YQLM
yqlYCryptoEncodeShaCallImpl _ _ (List args) = do
  case args of
   (ValueString message:_) -> do
     let hex = Crypto.encodeSha (fromString message)
     return . r.r.r.r.r.inj . LBS.toString $ hex
   _ -> newTypeErrorObject Nothing >>= jsThrow . inj

yqlYCryptoUUIDCallImpl :: InternalCallType YQLM
yqlYCryptoUUIDCallImpl _ _ _ = do
  uuid <- lift . lift $ Crypto.uuid
  return . r.r.r.r.r.inj . LBS.toString $ uuid

yqlYDateGetOffsetFromEpochInMillisCallImpl :: InternalCallType YQLM
yqlYDateGetOffsetFromEpochInMillisCallImpl = undefined

yqlYDecompressCallImpl :: InternalCallType YQLM
yqlYDecompressCallImpl _ _ (List args) = do
  case args of
   (ValueString s:_) -> do
     case Y.decompress (fromString s) of
      Just d -> return . r.r.r.r.r.inj . LBS.toString $ d
      _ -> jsThrow . r.r.r.r.inj $ "Invalid Base64 encoding"
   _ -> newTypeErrorObject Nothing >>= jsThrow . inj

yqlYDeflateCallImpl :: InternalCallType YQLM
yqlYDeflateCallImpl _ _ (List args) = do
  case args of
   (ValueString s:ValueNumber l:_) -> do
     return . r.r.r.r.r.inj . LBS.toString . flip Y.deflate (round l) . fromString $ s
   _ -> newTypeErrorObject Nothing >>= jsThrow . inj

yqlYExitCallImpl :: InternalCallType YQLM
yqlYExitCallImpl = undefined

yqlYIncludeCallImpl :: InternalCallType YQLM
yqlYIncludeCallImpl = undefined

yqlYInflateCallImpl :: InternalCallType YQLM
yqlYInflateCallImpl _ _ (List args) = do
  case args of
   (ValueString s:_) -> do
     case Y.inflate . fromString $ s of
      Just d -> return . r.r.r.r.r.inj . LBS.toString $ d
      _ -> jsThrow . r.r.r.r.inj $ "Invalid Base64 encoding"
   _ -> newTypeErrorObject Nothing >>= jsThrow . inj

yqlYJsToStringCallImpl :: InternalCallType YQLM
yqlYJsToStringCallImpl = undefined

yqlYRestCallImpl :: Object -> InternalCallType YQLM
yqlYRestCallImpl restObj f this (List args) = do
  case args of
   (ValueString url:ValueObject callback:_) -> do
     lift . lift . Y.rest $ url
     return . r.inj $ restObj
   _ -> newTypeErrorObject Nothing >>= jsThrow . inj

yqlYSyncCallImpl = undefined

yqlYTidyCallImpl = undefined

yqlYXparseJsonCallImpl = undefined
