-- | The Number List List encoding described in https://github.com/statebox/nll-spec.
module Data.NLL where

import Prelude
import Data.Array (uncons, snoc, take, drop)
import Data.Foldable (foldl, foldMap, null)
import Data.Maybe (Maybe(..), maybe)

-- | Example:
-- |
-- | `splitOn 0 [1,0,2,3,0,4,5,6]` would yield `[[1], [2,3], [4,5,6]]`.
-- |
-- | TODO:
-- |
-- | - Robustness in case of separator/no separator at the end of the input list.
-- | - Instead of `snoc`ing to the last element, we have this intermediate data stucture that should be less slow, but this is still ghastly.
splitOn :: ∀ a. Eq a => a -> Array a -> Array (Array a)
splitOn sep xs = finish $ foldl (f1 sep) { acc: [], lip: [] } xs
  where
    f1
      :: a
      -> { acc :: Array (Array a), lip :: Array a }
      -> a
      -> { acc :: Array (Array a), lip :: Array a }
    f1 separator { acc, lip } elem =
      if elem == separator
      then { acc: acc `snoc` lip, lip: [] }
      else { acc: acc           , lip: lip `snoc` elem }

    finish
      :: { acc :: Array (Array a), lip :: Array a }
      ->          Array (Array a)
    finish {acc, lip} =
      if null lip then acc else acc `snoc` lip

-- Approaches to sliding window maps:
--
-- - https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell
-- - https://twitter.com/gabrielg439/status/701460899589017600?lang=en
-- - 2-lists interpreted as Pairs: https://pursuit.purescript.org/packages/purescript-pairs/6.0.0/docs/Data.Pair
--
--   TODO:
--
--   - stack safety
--   - better name?

-- | A map on `Array`s with a 2-element wide sliding window.
-- |
-- | Example: `mapWindow2 Tuple [1,2,3,4]` would yield `[Tuple 1 2, Tuple 3 4]`.
mapWindow2 :: ∀ a b. (a -> a -> b) -> Array a -> Array b
mapWindow2 f xs = case xs of
  [] -> []
  _  -> maybe [] pure (map2aryTo2elemList f init) <> mapWindow2 f rest
  where
    init = take 2 xs
    rest = drop 2 xs

-- TODO handle illegal cases nicely (in type)
map2aryTo2elemList :: ∀ a b. (a -> a -> b) -> Array a -> Maybe b
map2aryTo2elemList f xs = case uncons xs of
  Nothing                           -> Nothing
  Just { head: head0, tail: tail0 } -> case uncons tail0 of
                                         Nothing                        -> Nothing
                                         Just { head: head1, tail: [] } -> Just (f head0 head1)
                                         Just { head: head1, tail: _  } -> Nothing
