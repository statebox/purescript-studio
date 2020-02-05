module GridKit.UIComponent where

import Prelude

import Data.Foldable (class Foldable, foldMap)
import Data.Newtype (alaF, un)
import Data.Ord.Min
import Data.Symbol
import Data.Variant
import Data.Vec3 (Point2)
import Data.Vec3.AffineTransform
import Halogen.HTML (PlainHTML)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeGet)
import Type.RowList
import Unsafe.Coerce (unsafeCoerce)

class UIComponent a where
  toSVG :: AffineTransform Number -> a -> Array PlainHTML
  distance :: Point2 Number -> a -> Number

toSVGFoldable :: ∀ t a. Foldable t => UIComponent a => AffineTransform Number -> t a -> Array PlainHTML
toSVGFoldable transform = foldMap (toSVG transform)

distanceFoldable :: ∀ t a. Foldable t => UIComponent a => Point2 Number -> t a -> Number
distanceFoldable pt = alaF Min foldMap (distance pt)

instance uiComponentArray :: UIComponent a => UIComponent (Array a) where
  toSVG = toSVGFoldable
  distance = distanceFoldable

instance uiComponentRecord :: (RowToList row list, AllUIComponents list row) => UIComponent (Record row) where
  toSVG transform = foldRecord (RLProxy :: RLProxy list) (toSVG transform)
  distance pt = foldRecord (RLProxy :: RLProxy list) (distance pt >>> Min) >>> un Min

instance uiComponentVariant :: (RowToList row list, AllUIComponents list row) => UIComponent (Variant row) where
  toSVG transform = foldVariant (RLProxy :: RLProxy list) (toSVG transform)
  distance pt = foldVariant (RLProxy :: RLProxy list) (distance pt)


class AllUIComponents (list :: RowList) (row :: # Type) | list -> row where
  foldRecord :: ∀ m. Monoid m => RLProxy list -> (∀ a. UIComponent a => a -> m) -> Record row -> m
  foldVariant :: ∀ m. RLProxy list -> (∀ a. UIComponent a => a -> m) -> Variant row -> m

instance allUIComponentsNil :: AllUIComponents RL.Nil () where
  foldRecord _ _ _ = mempty
  foldVariant _ _ = case_

instance allUiComponentsCons ::
  (AllUIComponents list tailRow, Row.Cons key value tailRow row, IsSymbol key, UIComponent value) =>
  AllUIComponents (RL.Cons key value list) row where
  foldRecord _ f r = f (get r) <> foldRecord (RLProxy :: RLProxy list) f (tail r)
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> value
      tail = unsafeCoerce :: Record row -> Record tailRow
  foldVariant _ f = on (SProxy :: SProxy key) f $ foldVariant (RLProxy :: RLProxy list) f
