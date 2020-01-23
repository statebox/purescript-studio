module Language.Statebox.Wiring.Generator.DiagramV2.Operators where

import Prelude
import Data.Array (findIndex, length, replicate, uncons, (!!), null)
import Data.Char (toCharCode)
import Data.Foldable (all, fold, intercalate)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.String (length) as S
import Data.String.CodeUnits (charAt, splitAt)
import Data.Vec3 (Vec3, _x, _y, _z)

import Language.Statebox.Wiring.Generator.DiagramV2

type Operator r =
  { label :: String
  , pos :: Vec3 Int
  | r
  }

fromOperators :: ∀ r. Array (Operator r) -> DiagramV2
fromOperators ops =
  { pixels : if null unconnected then pixels else unconnectedPixels <> "\n" <> pixels -- drawPixels ops
  , context : context <> "\n" <> unconnectedContext }
  where
    name i = (ops !! i) # maybe "" _.label
    isConnected srcPos tgtPos
         = _y tgtPos == _y srcPos + 1
        && srcStart < tgtEnd
        && tgtStart < srcEnd
      where
        srcStart = _x srcPos
        tgtStart = _x tgtPos
        srcEnd = srcStart + _z srcPos
        tgtEnd = tgtStart + _z tgtPos
    edges =
      ops # foldMapWithIndex \src { pos : srcPos } ->
        ops # foldMapWithIndex \tgt { pos : tgtPos } ->
          if isConnected srcPos tgtPos then [{ src, tgt }] else []
    { pixels, context } = fromEdges identity name edges
    unconnected = ops # foldMapWithIndex \i _ -> [i] # guard (edges # all (\{ src, tgt } -> src /= i && tgt /= i))
    unconnectedPixels = unconnected <#> nextChar 'a' # fold
    unconnectedContext = unconnected <#> (\i -> name i <> "@" <> nextChar 'a' i <> ": ->") # intercalate "\n"

pixel2operator :: ∀ r. Array (Operator r) -> String -> Maybe (Operator r)
pixel2operator ops pixelName = do
  pixelChar <- charAt 0 pixelName
  ops !! (toCharCode pixelChar - toCharCode 'a')

operator2pixel :: ∀ r. Array (Operator r) -> (Operator r -> Boolean) -> Maybe String
operator2pixel ops test =
  findIndex test ops <#> nextChar 'a'

drawPixels :: ∀ r. Array (Operator r) -> String
drawPixels ops = drawPixels' ops 0 [] # intercalate "\n"
  where
    drawPixels' arr i pixels' = case uncons arr of
      Nothing -> pixels'
      Just { head: { pos }, tail } ->
        drawPixels' tail (i + 1) pixels
          where
            xs = _x pos
            xe = xs + _z pos
            y = _y pos
            pixel = nextChar 'a' i
            pad 0 = ""
            pad n = " " <> pad (n - 1)
            addPixel l = let s = splitAt y l in s.before <> pad (max 0 (y - S.length l)) <> pixel <> s.after
            pixels'' :: Array String
            pixels'' = pixels' <> replicate (max 0 (xe - length pixels')) ""
            pixels :: Array String
            pixels = pixels'' # mapWithIndex \x l -> if x >= xs && x < xe then addPixel l else l
