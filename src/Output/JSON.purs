module Output.JSON where

import Prelude

import Data.Foldable (intercalate)

import Common
import Model (AnnPos, Brick, Term, TermF(..))

json :: Term AnnPos (Brick String) -> String
json = foldFix \(Ann _ f) -> alg f where
  alg TUnit = """{ "type": "unit" }"""
  alg (TBox { bid }) = """{ "type": "morphism", "name": """ <> "\"" <> bid <> "\"" <> """" }"""
  alg (TC terms) = """{ "type": "compose", "terms": [""" <> intercalate ", " terms <> """] }"""
  alg (TT terms) = """{ "type": "tensor", "terms": [""" <> intercalate ", " terms <> """] }"""
