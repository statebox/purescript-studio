module Language.Statebox where

import Prelude
import Data.Either (Either(..))
import Data.List (List(..))
import Text.Parsing.Parser (runParser, ParseError)

import Language.Statebox.AST (GElem(..))
import Language.Statebox.Parser as Parser

parseNet :: String -> Either ParseError (List GElem)
parseNet src = runParser src Parser.graph1
