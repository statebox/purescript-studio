module Svg.Util where

import Data.Vec2D (Vec3)
import Prelude ((<<<))
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

type CssSelector = String

foreign import _beginElements :: CssSelector -> EffectFnAff Int

beginElements :: CssSelector -> Aff Int
beginElements = fromEffectFnAff <<< _beginElements

foreign import domToSvgCoordinates :: Vec3 Int -> Vec3 Int
