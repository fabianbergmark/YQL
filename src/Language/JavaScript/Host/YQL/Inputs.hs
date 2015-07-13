{-# LANGUAGE PatternSynonyms,
             RecordWildCards #-}

module Language.JavaScript.Host.YQL.Inputs where

import Control.Applicative ((<$>))
import Control.Lens ((?=), use)
import Control.Monad (join, mzero, when)
import Control.Monad.Catch (throwM)

import Data.Map (Map)
import qualified Data.Map as Map (assocs, fromList, lookup)
import Data.Traversable (forM)

import Data.OpenDataTable
import Data.YQL (YQLException(..), YQLM)

import Language.JavaScript.Host
import Language.JavaScript.Interpret
import Language.JavaScript.SubType

inputs :: OpenDataTable -> Select -> Map String Primitive -> JavaScriptT YQLM ()
inputs OpenDataTable {..} Select {..} vs = do
  op <- use objectPrototypeObject
  fp <- use functionPrototypeObject

  inputsProperties <-
    join <$>
    forM selectInputs
    (\i -> do
        case i of
         InputKey InputInfo {..} -> do
           let key = maybe inputInfoId id inputInfoAs
           case Map.lookup key vs of
            Just v -> do
              let prop = (key, PropertyData DataDescriptor {
                             dataDescriptorValue          = r.inj $ v,
                             dataDescriptorWritable       = True,
                             dataDescriptorEnumerable     = False,
                             dataDescriptorConfigurable   = True })
              case (inputInfoType, v) of
               (InputTypeBool, PrimitiveBool _) -> return [prop]
               (InputTypeDate, PrimitiveString _) -> return [prop]
               (InputTypeDouble, PrimitiveNumber _) -> return [prop]
               (InputTypeInt, PrimitiveNumber _) -> return [prop]
               (InputTypeString, PrimitiveString _) -> return [prop]
               _ -> throwM YQLExceptionTypeError
            _ -> do
              when inputInfoRequired $
                throwM YQLExceptionMissingInput
              return mzero
         _ -> return mzero)

  yqlInputsId <- createNextInternalId
  let yqlInputsObj = Object yqlInputsId

      yqlInputsObjInt = ObjectInternal {
        objectInternalProperties        = Map.fromList inputsProperties,
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

  mInternalObject yqlInputsObj ?= yqlInputsObjInt

  forM (Map.assocs vs) $ \(key, value) -> do
    defineGlobalProperty
      key
      (PropertyData DataDescriptor {
          dataDescriptorValue          = r.inj $ value,
          dataDescriptorWritable       = True,
          dataDescriptorEnumerable     = False,
          dataDescriptorConfigurable   = True })

  defineGlobalProperty
    "inputs"
    (PropertyData DataDescriptor {
        dataDescriptorValue          = inj yqlInputsObj,
        dataDescriptorWritable       = True,
        dataDescriptorEnumerable     = False,
        dataDescriptorConfigurable   = True })
