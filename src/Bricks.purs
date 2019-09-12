module Bricks where
  
import Prelude

import Data.Array ((!!), length, slice, cons, snoc, filter, findIndex, zip, unzip, concat)
import Data.Foldable (and, foldMap)
import Data.List (snoc) as L 
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (alaF)
import Data.Ord.Max (Max(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

import Model
import Common


fromPixels :: ∀ bid. Ord bid => Array (Array bid) -> (bid -> Boolean) -> Bricks bid
fromPixels inp isHole = let term /\ boxes = findCuts false false 0 0 width height in { width, height, boxes, term }
  where

    at :: Int -> Int -> Maybe bid
    at x y = do
      row <- inp !! y
      row !! x
    height :: Int
    height = length inp
    width :: Int
    width = alaF Max foldMap length inp

    canCut :: Maybe bid -> Maybe bid -> Boolean
    canCut (Just a) (Just b) = a /= b || isHole a
    canCut _ _ = true

    vCuts :: Array (Array Boolean)
    vCuts = 1 ..< width <#> \x -> 0 ..< height <#> \y -> at (x - 1) y `canCut` at x y
    hCuts :: Array (Array Boolean)
    hCuts = 1 ..< height <#> \y -> 0 ..< width <#> \x -> at x (y - 1) `canCut` at x y

    toTerm :: Boolean -> Int -> Int -> Int -> Int -> Term Ann (Brick bid) /\ Array (Brick bid)
    toTerm false x0 y0 x1 y1 = let box = { topLeft: x0 /\ y0, bottomRight: x1 /\ y1 } in at x0 y0 
      # maybe (TUnit /\ []) (\bid -> let brick = { bid, box } in TBox brick /\ [brick])
    toTerm true y0 x0 y1 x1 = toTerm false x0 y0 x1 y1

    findCuts :: Boolean -> Boolean -> Int -> Int -> Int -> Int -> Term Ann (Brick bid) /\ Array (Brick bid)
    findCuts xySwapped didNotCut x0 y0 x1 y1 =
      case filter isCut ((y0 + 1) ..< y1) of
        [] -> if didNotCut then toTerm xySwapped x0 y0 x1 y1 else findCuts (not xySwapped) true y0 x0 y1 x1
        ys -> case unzip (zip (y0 `cons` ys) (ys `snoc` y1) <#> \(yl /\ yr) -> findCuts (not xySwapped) false yl x0 yr x1) of
          tms /\ bricks -> (if xySwapped then TC else TT) tms (y0 `cons` ys `snoc` y1) /\ concat bricks
      where
        isCut y = and $ slice x0 x1 $ fromMaybe [] $ (if xySwapped then vCuts else hCuts) !! (y - 1)

subTerm :: ∀ bid. Box -> Term Ann (Brick bid) -> Path -> Term Ann (Brick bid) /\ Selection
subTerm box (TC ts ann) p = subTerm' box ts ann p fst TC
subTerm box (TT ts ann) p = subTerm' box ts ann p snd TT
subTerm _ x path = x /\ { path, count: 1 }

subTerm'
  :: ∀ bid. Box -> Array (Term Ann (Brick bid)) -> Ann -> Path 
  -> (Int /\ Int -> Int) -> (Array (Term Ann (Brick bid)) -> Ann -> Term Ann (Brick bid))
  -> Term Ann (Brick bid) /\ Selection
subTerm' box ts ann p xOrY mkTerm = 
  if lb + 1 == ub 
    then (ts !! lb) # maybe (mkTerm ts ann /\ selection) \t -> subTerm box t (L.snoc p lb)
    else mkTerm (slice lb ub ts) (slice lb (ub + 1) ann) /\ selection
  where
    lb = maybe 0 (_ - 1) $ findIndex (_ > xOrY box.topLeft) ann
    ub = fromMaybe (length ann - 1) $ findIndex (_ >= xOrY box.bottomRight) ann
    selection = { path: L.snoc p lb, count: ub - lb }