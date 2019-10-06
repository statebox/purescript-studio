module Model where

import Prelude

import Data.Char (fromCharCode, toCharCode)
import Data.List (List)
import Data.Maybe (fromMaybe, maybe)
import Data.Map (Map)
import Data.Map as Map
import Data.String.CodeUnits (singleton)
import Data.String.Common (joinWith)
import Data.Traversable (mapAccumL)
import Data.Tuple.Nested (type (/\))

import Common

data TermF ann brick r
  = TUnit
  | TBox brick
  | TC (Array r) ann -- ^ Composition
  | TT (Array r) ann -- ^ Tensor

type Term ann brick = Fix (TermF ann brick)

tunit :: ∀ ann brick. Term ann brick
tunit = Fix TUnit

tbox :: ∀ ann brick. brick -> Term ann brick
tbox brick = Fix (TBox brick)

tc :: ∀ ann brick. Array (Term ann brick) -> ann -> Term ann brick
tc ts ann = Fix (TC ts ann)

tt :: ∀ ann brick. Array (Term ann brick) -> ann -> Term ann brick
tt ts ann = Fix (TT ts ann)

instance functorTermF :: Functor (TermF ann brick) where
  map _ TUnit = TUnit
  map _ (TBox box) = TBox box
  map f (TC ts ann) = TC (map f ts) ann
  map f (TT ts ann) = TT (map f ts) ann


type Box =
  { topLeft :: Int /\ Int
  , bottomRight :: Int /\ Int
  }
type Brick bid = { bid :: bid, box :: Box }
type Bricks bid =
  { width :: Int
  , height :: Int
  , boxes :: Array (Brick bid)
  , term :: Term AnnPos (Brick bid)
  }

type AnnPos = Array Int

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

type TypedTerm ann brick bv = Fix (Ann (Ty (VarWithBox bv)) (TermF ann brick))
