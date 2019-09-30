module Output.JSON where

import Prelude

import Data.Foldable (intercalate)

import Model (Ann, Brick, Term(..))

json :: Term Ann (Brick String) -> String
json TUnit = """{ "type": "unit" }"""
json (TBox { bid }) = """{ "type": "morphism", "name": """ <> "\"" <> bid <> "\"" <> """" }"""
json (TC terms _) = """{ "type": "compose", "terms": [""" <> intercalate ", " (map json terms) <> """] }"""
json (TT terms _) = """{ "type": "tensor", "terms": [""" <> intercalate ", " (map json terms) <> """] }"""