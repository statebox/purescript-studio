module Language.Statebox where

import Prelude
import Data.Either (Either(..))
import Data.List (List(..))
import Text.Parsing.Parser (runParser, ParseError)

import Language.Statebox.AST (GElem1(..))
import Language.Statebox.Parser as Parser

parseNet :: String -> Either ParseError (List GElem1)
parseNet src = runParser src Parser.graph1
