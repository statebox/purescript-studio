module Bricks where
  
import Prelude

import Data.Array ((!!), length, slice, cons, snoc, filter, findIndex, zip, unzip, unsafeIndex, concat)
import Data.Either (Either(..), note, either)
import Data.Foldable (and, foldMap)
import Data.List (snoc, last) as L 
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (alaF)
import Data.Ord.Max (Max(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

import Model
import Common


fromPixels :: ∀ bid. Ord bid => Array (Array bid) -> (bid -> Boolean) -> Bricks bid
fromPixels inp isHole = let term /\ boxes = findHCuts false 0 0 width height in { width, height, boxes, term }
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

    toTerm :: Int -> Int -> Int -> Int -> Term Ann (Brick bid) /\ Array (Brick bid)
    toTerm x0 y0 x1 y1 = let box = { topLeft: x0 /\ y0, bottomRight: x1 /\ y1 } in at x0 y0 
      # maybe (TUnit /\ []) (\bid -> let brick = { bid, box } in TBox brick /\ [brick])

    -- TODO dedupe these?
    findHCuts :: Boolean -> Int -> Int -> Int -> Int -> Term Ann (Brick bid) /\ Array (Brick bid)
    findHCuts didNotCut x0 y0 x1 y1 =
      case filter isCut ((y0 + 1) ..< y1) of
        [] -> if didNotCut then toTerm x0 y0 x1 y1 else findVCuts true x0 y0 x1 y1
        ys -> case unzip (zip (y0 `cons` ys) (ys `snoc` y1) <#> \(yl /\ yr) -> findVCuts false x0 yl x1 yr) of
          tms /\ bricks -> TT tms (y0 `cons` ys `snoc` y1) /\ concat bricks
      where
        isCut y = and $ slice x0 x1 $ fromMaybe [] $ hCuts !! (y - 1)
    
    findVCuts :: Boolean -> Int -> Int -> Int -> Int -> Term Ann (Brick bid) /\ Array (Brick bid)
    findVCuts didNotCut x0 y0 x1 y1 = 
      case filter isCut ((x0 + 1) ..< x1) of
        [] -> if didNotCut then toTerm x0 y0 x1 y1 else findHCuts true x0 y0 x1 y1
        xs -> case unzip (zip (x0 `cons` xs) (xs `snoc` x1) <#> \(xl /\ xr) -> findHCuts false xl y0 xr y1) of
          tms /\ bricks -> TC tms (x0 `cons` xs `snoc` x1) /\ concat bricks
      where
        isCut x = and $ slice y0 y1 $ fromMaybe [] $ vCuts !! (x - 1)

subTerm :: ∀ bid. Box -> Term Ann (Brick bid) /\ Path -> Term Ann (Brick bid) /\ Path /\ Int
subTerm box (TT ts ys /\ p) = if lb + 1 == ub 
    then maybe (TT ts ys /\ L.snoc p lb /\ (ub - lb)) (\t -> subTerm box (t /\ L.snoc p lb)) (ts !! lb) 
    else TT (slice lb ub ts) (slice lb (ub + 1) ys) /\ L.snoc p lb /\ (ub - lb)
  where
    lb = maybe 0 (_ - 1) $ findIndex (_ > snd box.topLeft) ys
    ub = fromMaybe (length ys - 1) $ findIndex (_ >= snd box.bottomRight) ys
subTerm box (TC ts xs /\ p) = if lb + 1 == ub 
    then maybe (TC ts xs /\ L.snoc p lb /\ (ub - lb)) (\t -> subTerm box (t /\ L.snoc p lb)) (ts !! lb) 
    else TC (slice lb ub ts) (slice lb (ub + 1) xs) /\ L.snoc p lb /\ (ub - lb)
  where
    lb = maybe 0 (_ - 1) $ findIndex (_ > fst box.topLeft) xs
    ub = fromMaybe (length xs - 1) $ findIndex (_ >= fst box.bottomRight) xs
subTerm _ (x /\ p) = x /\ p /\ 1