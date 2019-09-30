module Model where

import Prelude

import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (class Foldable, foldMap, foldrDefault, foldlDefault)
import Data.List (List)
import Data.Maybe (fromMaybe, maybe)
import Data.Map (Map)
import Data.Map as Map
import Data.String.CodeUnits (singleton)
import Data.String.Common (joinWith)
import Data.Traversable (mapAccumL)
import Data.Tuple.Nested (type (/\))

data Term ann brick
  = TUnit
  | TBox brick
  | TC (Array (Term ann brick)) ann -- ^ Composition
  | TT (Array (Term ann brick)) ann -- ^ Tensor

instance foldableTerm :: Foldable (Term ann) where
  foldr f z = foldrDefault f z
  foldl f z = foldlDefault f z
  foldMap f = go
    where
      go TUnit = mempty
      go (TBox box) = f box
      go (TC ts _) = foldMap go ts
      go (TT ts _) = foldMap go ts

type Box =
  { topLeft :: Int /\ Int
  , bottomRight :: Int /\ Int
  }
type Brick bid = { bid :: bid, box :: Box }
type Bricks bid =
  { width :: Int
  , height :: Int
  , boxes :: Array (Brick bid)
  , term :: Term Ann (Brick bid)
  }

type Ann = Array Int

type Selection = { path :: Path, count :: Int }
type Path = List Int


data Ty bid = Ty (Array bid) (Array bid)

derive instance eqTy :: (Eq bv) => Eq (Ty bv)
instance semigroupTy :: Semigroup (Ty bv) where
  append (Ty bid b) (Ty c d) = Ty (bid <> c) (b <> d)
instance monoidTy :: Monoid (Ty bv) where
  mempty = Ty [] []
instance showTyVarString :: (Show (Var bv)) => Show (Ty (Var bv)) where
  show (Ty ls rs) = showTypes ls " -> " rs

showTypes :: ∀ bv. Show (Var bv) => Array (Var bv) -> String -> Array (Var bv) -> String
showTypes ls middle rs =
  joinWith " " l.value <> middle <>
  joinWith " " r.value
  where
    l = mapAccumL replFv Map.empty ls
    r = mapAccumL replFv l.accum rs
    replFv m (FreeVar fv) = Map.lookup fv m # maybe new old
      where
        old name = { accum: m, value: name }
        new = let next = singleton $ fromMaybe 'α' $ fromCharCode (toCharCode 'α' + Map.size m) in
          { accum: Map.insert fv next m, value: next }
    replFv m bv = { accum: m, value: show bv }


data Color = White | Black
instance showColor :: Show Color where
  show White = "white"
  show Black = "black"

data TypeDecl bv
  = Perm (Array Int)
  | Spider Color Int Int
  | Gen (Ty bv)

isGen :: ∀ bv. TypeDecl bv -> Boolean
isGen (Gen _) = true
isGen _ = false

type Context bv bid = Map bid (TypeDecl bv)


data Var bv
  = FreeVar (Int /\ Box) -- Easy way to generate unique free variables from boxes
  | BoundVar bv

instance eqVar :: (Eq bv) => Eq (Var bv) where
  eq (FreeVar l) (FreeVar r) = eq l r
  eq (BoundVar l) (BoundVar r) = eq l r
  eq _ _ = false

instance showVarString :: Show (Var String) where
  show (FreeVar _) = "α"
  show (BoundVar bv) = bv

type VarWithBox bv = { box :: Box, var :: Var bv }

data Side = Input | Output
derive instance eqSide :: Eq Side
derive instance ordSide :: Ord Side

data Validity = Valid | Invalid
derive instance eqValidity :: Eq Validity

data Matches a = Matched (Array (Validity /\ a /\ a)) | Unmatched Validity Side (Array a)

type Match bv = { y :: Number, validity :: Validity, object :: bv }
