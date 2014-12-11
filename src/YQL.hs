{-# LANGUAGE RecordWildCards,
             PatternSynonyms #-}

module YQL
       ( runYQL ) where

import Control.Lens
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.State.Lazy (evalStateT)

import Control.Applicative ((<$>))
import Control.Monad.Trans.Except (catchE)

import qualified Data.Aeson as Aeson
import Data.Default (def)
import Data.Foldable (foldlM, foldrM)
import qualified Data.HashMap.Strict as HashMap (empty, insert)
import Data.Map (Map)
import qualified Data.Map as Map (foldlWithKey, toList)
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Data.Text as Text (pack)
import qualified Data.Text.Lazy as Text (toStrict)
import Data.Text.Lazy.Encoding
import qualified Data.Vector as Vector (cons, empty)

import Network.HTTP.Conduit (parseUrl)
import Network.HTTP.Client.Conduit (newManager)
import Network.URI.Template

import System.IO (stderr, Handle)
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

import Data.OpenDataTable
import Data.YQL
import Data.YQL.Response
import Data.YQL.Rest
import Data.YQL.Result

import qualified Language.JavaScript as JS
import qualified Language.JavaScript.Host.YQL as JS
import qualified Language.JavaScript.Host.Console as JS
import qualified Language.JavaScript.Interpret as JS
import qualified Language.JavaScript.Parser as JS

import YQL.Rest
import YQL.Y

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    where formatter = simpleLogFormatter "[$time $loggername $prio] $msg"

runYQL :: OpenDataTable -> Map String JS.Primitive -> IO Aeson.Value
runYQL ot@(OpenDataTable {..}) vs = do
  let logName = rootLoggerName
  stderrHandler <- withFormatter <$> streamHandler stderr DEBUG
  updateGlobalLogger logName (setLevel DEBUG)
  updateGlobalLogger logName (setHandlers [stderrHandler])

  let selects = [s | SelectBinding s <- openDataTableBindings]
  case selects of
   (s@(Select{..}):_) -> do
     manager <- newManager
     let env =
           Map.foldlWithKey
           (\e k v -> addToEnv k (JS.primitiveToString v) e)
           newEnv vs
     request <- maybe (return def) (parseUrl . expand env) selectUrl
     let _rest = Rest {
           restHttpManager = manager,
           restHttpRequest = request }
         _y = undefined
         _yql = YQL {
           yqlRest = _rest,
           yqlY = _y }
         mSource = openDataTableExecute <> selectExecute
     flip evalStateT _yql $ do
       case mSource of
        Just source -> do
          let eAST = JS.parseJavaScript source
          case eAST of
           Right ast -> do
             let hostInit = JS.console >> JS.yql ot s vs
             eRes <- JS.runJavaScriptT JS.initialState $ do
               responseObj <- hostInit

               (JS.interpret ast :: JS.JavaScriptT YQLM JS.Completion)
                 `catchE`
                 (\ v -> do
                     s <- JS.toString v
                     throwM $ YQLExceptionJSRuntimeError s)

               oi <- use $ JS.internalObject responseObj
               case oi ^. JS.internalProperty "object" of
                Just (JS.PropertyData JS.DataDescriptor {..}) ->
                  valueToJSON dataDescriptorValue
                _ -> return Aeson.Null
             case eRes of
              Right res -> return res
              Left _ -> do
                throwM YQLExceptionInternalError
           Left e -> throwM $ YQLExceptionJSParseError e
        _ -> do
          Result {..} <- get
          case resultResponse of
           ResponseByteString s -> do
             return $ Aeson.String (Text.toStrict . decodeUtf8 $ s)
           ResponseJSON j -> return j
   _ -> throwM YQLExceptionMissingSelect

valueToJSON :: (Functor m, Monad m) => JS.Value -> JS.JavaScriptT m Aeson.Value
valueToJSON (JS.ValueNull _) = return Aeson.Null
valueToJSON (JS.ValueUndefined _) = return Aeson.Null
valueToJSON (JS.ValueNumber n) =
  return $ Aeson.Number (fromRational . toRational $ n)
valueToJSON (JS.ValueString s) = return $ Aeson.String (fromString s)
valueToJSON (JS.ValueBool b) = return $ Aeson.Bool b
valueToJSON (JS.ValueObject o) = do
  c <- use $ JS.class' o
  case c of
   "Object" -> do
     ps <- use $ JS.properties o
     jo <- foldlM addObjectField (HashMap.empty) (Map.toList ps)
     return $ Aeson.Object jo

   "Array" -> do
     ps <- use $ JS.properties o
     ja <- foldrM addArrayItem (Vector.empty) (Map.toList ps)
     return $ Aeson.Array ja

  where
       addObjectField :: (Functor m, Monad m) =>
                         Aeson.Object -> (String, JS.Property) ->
                         JS.JavaScriptT m Aeson.Object
       addObjectField jo (name, property) =
         case property of
          JS.PropertyData (JS.DataDescriptor {..}) -> do
            if dataDescriptorEnumerable
              then do
              j <- valueToJSON dataDescriptorValue
              return $ HashMap.insert (Text.pack name) j jo
              else return jo
          _ -> return jo

       addArrayItem :: (Functor m, Monad m) =>
                       (String, JS.Property) -> Aeson.Array ->
                       JS.JavaScriptT m Aeson.Array
       addArrayItem (name, property) ja = do
         if name /= "length"
           then do
           case property of
            JS.PropertyData (JS.DataDescriptor {..}) -> do
              if dataDescriptorEnumerable
                then do
                j <- valueToJSON dataDescriptorValue
                return $ Vector.cons j ja
                else return ja
            _ -> return ja
           else return ja
