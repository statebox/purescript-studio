module Language.Statebox where

import Prelude
import Data.Either (Either(..))
import Data.List (List(..))
import Text.Parsing.Parser (runParser, ParseError)

import Language.Statebox.Net.AST (GElem(..)) as Net
import Language.Statebox.Net.Parser as NetParser
import Language.Statebox.Wiring.AST (GElem(..)) as Wiring
import Language.Statebox.Wiring.Parser as WiringParser

parseNet :: String -> Either ParseError (List Net.GElem)
parseNet src = runParser src NetParser.net

parseDiagram :: String -> Either ParseError (List Wiring.GElem)
parseDiagram src = runParser src WiringParser.diagram
