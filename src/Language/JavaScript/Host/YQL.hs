module Language.JavaScript.Host.YQL where

import Data.Map (Map)

import Data.OpenDataTable
import Data.YQL (YQLM)

import Language.JavaScript.Interpret
import Language.JavaScript.Host.YQL.Inputs
import Language.JavaScript.Host.YQL.Rest
import Language.JavaScript.Host.YQL.Response
import Language.JavaScript.Host.YQL.Y

yql :: OpenDataTable -> Select -> Map String Primitive ->  JavaScriptT YQLM Object
yql ot s vs = inputs ot s vs >> request ot s >>= y ot s >> response
