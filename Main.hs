{-# LANGUAGE RankNTypes,
             RecordWildCards #-}

module Main
       ( main ) where

import Control.Applicative ((<$>))
import Control.Monad (forM, join, mzero)
import Control.Monad.Catch

import qualified Data.Map as Map (fromList)
import Data.Maybe (listToMaybe)

import System.IO

import Text.XML.HXT.Core (runX, readString, withRemoveWS, yes, (>>>))

import System.Environment (getArgs)

import Data.OpenDataTable
import Data.YQL

import Language.JavaScript.Interpret
import Language.JavaScript.SubType

import Data.OpenDataTable.Parser

import YQL

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (xml:_) <- getArgs
  xmlFile <- openFile xml ReadMode
  document <- hGetContents xmlFile

  mOpenDataTable <- listToMaybe <$> runX (readString [withRemoveWS yes] document
                                          >>> parseOpenDataTable)
  case mOpenDataTable of
   Just opentable@(OpenDataTable {..}) -> do
     let selects = [s | SelectBinding s <- openDataTableBindings]
     case selects of
      Select {..}:_ -> do
        vs <- join <$>
          forM selectInputs
          (\i -> do
              case i of
               InputKey InputInfo {..} -> do
                 let key = maybe inputInfoId id inputInfoAs
                 putStr (key ++ ": ")
                 str <- getLine
                 let value = case inputInfoType of
                              InputTypeBool -> inj (read str :: Bool)
                              InputTypeDate -> inj str
                              InputTypeDouble -> inj (Number (read str :: Double))
                              InputTypeFloat -> inj (Number (read str :: Double))
                              InputTypeInt -> inj (Number (read str :: Double))
                              InputTypeString -> inj str
                 return [(key, value)]
               _ -> return mzero)
        result <- runYQL opentable (Map.fromList vs)
        print result
      _ -> throwM YQLExceptionMissingSelect
   _ -> throwM YQLExceptionXMLError
