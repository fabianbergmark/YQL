module Language.JavaScript.Host.YQL.Response where

import Control.Lens ((?=), use)

import Data.YQL (YQLM)

import Language.JavaScript.Host
import Language.JavaScript.Interpret
import Language.JavaScript.SubType

import qualified Data.Map as Map (empty)

response :: JavaScriptT YQLM Object
response = do
  op <- use objectPrototypeObject
  fp <- use functionPrototypeObject

  yqlResponseId <- createNextInternalId
  let yqlResponseObj = Object yqlResponseId
      yqlResponseObjInt = ObjectInternal {
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

  mInternalObject yqlResponseObj ?= yqlResponseObjInt

  defineGlobalProperty
    "response"
    (PropertyData DataDescriptor {
        dataDescriptorValue          = inj yqlResponseObj,
        dataDescriptorWritable       = True,
        dataDescriptorEnumerable     = False,
        dataDescriptorConfigurable   = True })

  return yqlResponseObj
