module KDMonCat.API where

import Prelude
import Data.Either
import Effect
import Effect.Exception

import KDMonCat.Bricks as Bricks
import KDMonCat.InferType
import KDMoncat.Input.String
import KDMonCat.Output.JSON

type Error = String

type JSON = String

type Input =
  { pixels :: String
  , context :: String
  }

toJSON :: Input -> Effect JSON
toJSON input =
  envE # either (error >>> throwException) (\env -> pure $ json (inferType env bricks.term).term)
  where
    envE = parseContext input.context
    bricks = Bricks.fromPixels (parsePixels input.pixels) (\s -> s == " " || s == "-" || s == "=")
