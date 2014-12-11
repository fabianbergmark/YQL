{-# LANGUAGE DeriveDataTypeable,
             TemplateHaskell #-}

module Data.YQL
       ( YQL(..)
       , YQLException(..)
       , YQLM
       , rest
       , y ) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Trans.State.Lazy (StateT)

import Data.Typeable

import Text.Parsec (ParseError)

import Data.YQL.Response
import Data.YQL.Rest
import Data.YQL.Y

import Language.JavaScript.Interpret (Value)

data YQL =
  YQL
  { yqlResponse :: Response
  , yqlRest     :: Rest
  , yqlY        :: Y }

type YQLM = StateT YQL IO

data YQLException
  = YQLExceptionInternalError
  | YQLExceptionJSParseError ParseError
  | YQLExceptionJSRuntimeError String
  | YQLExceptionJSONError
  | YQLExceptionMissingInput
  | YQLExceptionMissingSelect
  | YQLExceptionTypeError
  | YQLExceptionXMLError
  deriving (Show, Typeable)

instance Exception YQLException

response :: Lens' YQL Response
response = lens yqlResponse (\yql r -> yql { yqlResponse = r })

rest :: Lens' YQL Rest
rest = lens yqlRest (\yql r -> yql { yqlRest = r })

y :: Lens' YQL Y
y = lens yqlY (\yql y' -> yql { yqlY = y' })
