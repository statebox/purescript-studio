module KDMonCat.API where

import Prelude
import Data.Either
import Effect
import Effect.Exception

import KDMonCat.Bricks as Bricks
import KDMonCat.InferType
import KDMoncat.Input.String as String
import KDMonCat.Output.JSON

type Error = String

type JSON = String

toJSON :: String.Input -> Effect JSON
toJSON input =
  envE # either (error >>> throwException) (\env -> pure $ json (inferType env bricks.term).term)
  where
    envE = String.parseContext input.context
    bricks = Bricks.fromPixels (String.parsePixels input.pixels) (\s -> s == " " || s == "-" || s == "=")
