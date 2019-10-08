module Model where

import Prelude

import Data.Bifunctor
import Data.Bitraversable
import Data.Char (fromCharCode, toCharCode)
import Data.List (List)
import Data.Maybe (fromMaybe, maybe, Maybe(Nothing))
import Data.Map (Map)
import Data.Map as Map
import Data.String.CodeUnits (singleton)
import Data.String (joinWith, stripSuffix, Pattern(..))
import Data.Traversable
import Data.Tuple.Nested (type (/\))

import Common

data TermF brick r
  = TUnit
  | TBox brick
  | TC (Array r) -- ^ Composition
  | TT (Array r) -- ^ Tensor

type Term ann brick = Fix (Ann ann TermF) brick

tunit :: ∀ ann brick. Monoid ann => Term ann brick
tunit = Fix (Ann mempty TUnit)

tbox :: ∀ ann brick. Monoid ann => brick -> Term ann brick
tbox brick = Fix (Ann mempty (TBox brick))

tc :: ∀ ann brick. Array (Term ann brick) -> ann -> Term ann brick
tc ts ann = Fix (Ann ann (TC ts))

tt :: ∀ ann brick. Array (Term ann brick) -> ann -> Term ann brick
tt ts ann = Fix (Ann ann (TT ts))

instance functorTermF :: Functor (TermF brick) where
  map = bimap identity
instance foldmapTermF :: Foldable (TermF brick) where
  foldMap = bifoldMap (const mempty)
  foldr f z = foldrDefault f z
  foldl f z = foldlDefault f z
instance traversableTermF :: Traversable (TermF brick) where
  traverse = bitraverse pure
  sequence = sequenceDefault
instance bifunctorTermF :: Bifunctor TermF where
  bimap _ _ TUnit = TUnit
  bimap f _ (TBox box) = TBox (f box)
  bimap _ g (TC ts) = TC (map g ts)
  bimap _ g (TT ts) = TT (map g ts)
instance bifoldableTermF :: Bifoldable TermF where
  bifoldMap _ _ TUnit = mempty
  bifoldMap f _ (TBox box) = f box
  bifoldMap _ g (TC ts) = foldMap g ts
  bifoldMap _ g (TT ts) = foldMap g ts
  bifoldr f g = bifoldrDefault f g
  bifoldl f g = bifoldlDefault f g
instance bitraversableTermF :: Bitraversable TermF where
  bitraverse _ _ TUnit = pure TUnit
  bitraverse f _ (TBox box) = TBox <$> f box
  bitraverse _ g (TC ts) = TC <$> traverse g ts
  bitraverse _ g (TT ts) = TT <$> traverse g ts
  bisequence = bisequenceDefault


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
derive instance functorTy :: Functor Ty
instance foldableTy :: Foldable Ty where
  foldMap f (Ty l r) = foldMap f l <> foldMap f r
  foldr f z = foldrDefault f z
  foldl f z = foldlDefault f z
instance traversableTy :: Traversable Ty where
  traverse f (Ty l r) = Ty <$> traverse f l <*> traverse f r
  sequence = sequenceDefault
instance semigroupTy :: Semigroup (Ty bv) where
  append (Ty bid b) (Ty c d) = Ty (bid <> c) (b <> d)
instance monoidTy :: Monoid (Ty bv) where
  mempty = Ty [] []
instance showTyVarString :: (Show (Var bv)) => Show (Ty (Var bv)) where
  show (Ty ls rs) = showTypes ls " -> " rs
instance showTyString :: Show (Ty String) where
  show (Ty ls rs) = joinWith " " ls <> " -> " <> joinWith " " rs

showTypes :: ∀ bv. Show (Var bv) => Array (Var bv) -> String -> Array (Var bv) -> String
showTypes ls middle rs = joinWith " " l.value <> middle <> joinWith " " r.value
  where
    l = varsToString Map.empty ls
    r = varsToString l.accum rs

varsToString
  :: ∀ bv t. Show (Var bv) => Traversable t
  => Map (Int /\ Box) String -> t (Var bv) -> Accum (Map (Int /\ Box) String) (t String)
varsToString = mapAccumL replFv
  where
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

type TypedTerm bv bid = Fix (Ann (Ty String) TermF) { bid :: bid, box :: Box, decl :: TypeDecl bv }

isBackwards :: String -> Boolean
isBackwards s = stripSuffix (Pattern "*") s /= Nothing
