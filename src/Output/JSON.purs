module Output.JSON where

import Prelude

import Data.Foldable (intercalate)

import Common
import Model (AnnPos, Brick, Term, TermF(..))

json :: Term AnnPos (Brick String) -> String
json = foldFix alg where
  alg TUnit = """{ "type": "unit" }"""
  alg (TBox { bid }) = """{ "type": "morphism", "name": """ <> "\"" <> bid <> "\"" <> """" }"""
  alg (TC terms _) = """{ "type": "compose", "terms": [""" <> intercalate ", " terms <> """] }"""
  alg (TT terms _) = """{ "type": "tensor", "terms": [""" <> intercalate ", " terms <> """] }"""
