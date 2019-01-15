module Svg.Util where

import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Prelude ((<<<))
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

type CssSelector = String

foreign import _beginElements :: CssSelector -> EffectFnAff Int

beginElements :: CssSelector -> Aff Int
beginElements = fromEffectFnAff <<< _beginElements

foreign import domToSvgCoordinates :: Int /\ Int -> Int /\ Int
