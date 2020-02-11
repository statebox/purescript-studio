module KDMonCat.API where

import Prelude
import Data.Either
import Data.Foldable
import Effect
import Effect.Exception
import Data.Tuple.Nested (type (/\), (/\))

import KDMonCat.Bricks as Bricks
import KDMonCat.InferType
import KDMonCat.Model
import KDMoncat.Input.String as String
import KDMonCat.Output.JSON

type Error = String

type JSON = String

toJSON :: String.Input -> Effect JSON
toJSON input =
  envE # either (error >>> throwException) (\env -> inferJSON env bricks.term)
  where
    envE = String.parseContext input.context
    bricks = Bricks.fromPixels (String.parsePixels input.pixels) (\s -> s == " " || s == "-" || s == "=")

inferJSON env untypedTerm = if null matchErrors
    then pure $ json term
    else matchErrors # intercalate "\n" >>> error >>> throwException
  where
    { term, matches } = inferType env untypedTerm
    matchErrors = matches # foldMap toError

    toError :: Matches (VarWithBox String) -> Array String
    toError (Unmatched Invalid side arr) = ["unmatched " <> show side <> " (" <> intercalate " " (map (_.var >>> show) arr) <> ")"]
    toError (Unmatched _ _ _) = []
    toError (Matched arr) = arr # foldMap toErrorMatched

    toErrorMatched :: (Validity /\ VarWithBox String /\ VarWithBox String) -> Array String
    toErrorMatched (Invalid /\ l /\ r) = ["type error between " <> show l.var <> " and " <> show r.var]
    toErrorMatched _ = []
