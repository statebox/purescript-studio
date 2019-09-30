module InferType where

import Prelude

import Data.Array (zip, uncons, filter)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl, length)
import Data.Functor.App (App(..))
import Data.Int (floor)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (alaF)
import Data.String (trim, split, joinWith)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple.Nested (type (/\), (/\))
import Global (readInt)

import Model
import Common ((..<))


type InferredType bv =
  { type :: Ty (VarWithBox bv)
  , matches :: Array (Matches (VarWithBox bv))
  , errors :: Array String
  }

newtype Bounds bv = Bounds (Map (Int /\ Box) (Var bv))
instance boundsSemigroup :: Semigroup (Bounds bv) where
  append (Bounds l) (Bounds r) = let l' = l <#> replace (Bounds r) in Bounds (l' <> (r <#> replace (Bounds l')))
instance boundsMonoid :: Monoid (Bounds bv) where
  mempty = Bounds (Map.empty)

getType :: ∀ bv. InferredType bv -> Ty (Var bv)
getType { type: Ty l r } = Ty (l <#> _.var) (r <#> _.var)

empty :: ∀ bv. InferredType bv
empty = mempty

note :: ∀ bv x. String -> (x -> InferredType bv) -> Maybe x -> InferredType bv
note s = maybe (empty { errors = [s] })

showInferred :: ∀ bv. Show (Var bv) => InferredType bv -> String
showInferred it@{ errors } = if length errors == 0 then show (getType it) else joinWith "\n" errors


inferType
  :: ∀ bid ann bv. Ord bid => Show bid => Eq bv => Show (Var bv)
  => Term ann (Brick bid) -> Context bv bid -> InferredType bv
inferType TUnit _ = empty
inferType (TBox { box, bid }) ctx =
  Map.lookup bid ctx # note ("Undeclared name: " <> show bid) (inferBoxType box)
inferType (TT ts _) ctx = foldMap inferType ts ctx
inferType (TC tts _) ctx = uncons tts #
  note "Composition of empty list not supported" \{ head, tail } ->
  foldl compose (inferType head ctx) tail
    where
      compose :: Show (Var bv) => InferredType bv -> Term ann (Brick bid) -> InferredType bv
      compose acc@{ type: Ty a b } term = res
        where
          step@{ type: Ty b' c } = inferType term ctx
          res = if ((length b :: Int) /= length b')
            then
              (acc <> step <> empty { matches = [Unmatched Invalid Output b, Unmatched Invalid Input b'] }) { type = Ty a c }
            else let { bounds, matches } = foldl bindVars mempty (zip b b') in
              (acc <> step)
                { type = Ty (a <#> replaceBoxed bounds) (c <#> replaceBoxed bounds)
                , matches = replaceMatches bounds <$> acc.matches <> [Matched matches]
                }

inferBoxType :: ∀ bv. Box -> TypeDecl bv -> InferredType bv
inferBoxType box (Gen (Ty i o)) = empty { type = Ty (i <#> \bv -> { box, var: BoundVar bv }) (o <#> \bv -> { box, var: BoundVar bv }) }
inferBoxType box (Perm perm) =
  empty { type = Ty (0 ..< length perm <#> (_ + 1) <#> \i -> { box, var: FreeVar (i /\ box) }) (perm <#> \i -> { box, var: FreeVar (i /\ box) }) }
inferBoxType box (Spider _ l r) =
  empty { type = Ty (0 ..< l <#> const { box, var: FreeVar (0 /\ box) }) (0 ..< r <#> const { box, var: FreeVar (0 /\ box) }) }

bindVars
  :: ∀ bv. Eq bv => Show (Var bv)
  => { bounds :: Bounds bv, matches :: Array (Validity /\ VarWithBox bv /\ VarWithBox bv) }
  -> VarWithBox bv /\ VarWithBox bv
  -> { bounds :: Bounds bv, matches :: Array (Validity /\ VarWithBox bv /\ VarWithBox bv) }
bindVars bm@{ bounds, matches } (l /\ r) = bm <> bindVars' (replaceBoxed bounds l /\ replaceBoxed bounds r)

bindVars'
  :: ∀ bv. Eq bv => Show (Var bv)
  => VarWithBox bv /\ VarWithBox bv
  -> { bounds :: Bounds bv, matches :: Array (Validity /\ VarWithBox bv /\ VarWithBox bv) }
bindVars' (ml@{ var: l } /\ { box, var: FreeVar r })   = { bounds: Bounds (Map.singleton r l), matches: [Valid /\ ml /\ { box, var: l }] }
bindVars' ({ box, var: FreeVar l } /\ mr@{ var: r })   = { bounds: Bounds (Map.singleton l r), matches: [Valid /\ { box, var: r } /\ mr] }
bindVars' m@({ var: l } /\ { var: r }) | l == r        = { bounds: mempty, matches: [Valid /\ m] }
                                       | otherwise     = { bounds: mempty, matches: [Invalid /\ m] }

replaceMatches :: ∀ bv. Bounds bv -> Matches (VarWithBox bv) -> Matches (VarWithBox bv)
replaceMatches bounds (Matched ms) = Matched (ms <#> replaceMatched bounds)
replaceMatches bounds (Unmatched v io vs) = Unmatched v io (vs <#> replaceBoxed bounds)

replaceMatched :: ∀ bv. Bounds bv -> Validity /\ VarWithBox bv /\ VarWithBox bv -> Validity /\ VarWithBox bv /\ VarWithBox bv
replaceMatched bounds (val /\ l /\ r) = val /\ replaceBoxed bounds l /\ replaceBoxed bounds r

replaceBoxed :: ∀ bv. Bounds bv -> VarWithBox bv -> VarWithBox bv
replaceBoxed bounds { box, var } = { box, var: replace bounds var }

replace :: ∀ bv. Bounds bv -> Var bv -> Var bv
replace _ (BoundVar bv) = BoundVar bv
replace (Bounds bounds) (FreeVar fv) = Map.lookup fv bounds # fromMaybe (FreeVar fv)


defaultEnv :: Context String String
defaultEnv = Map.fromFoldable
  [ " " /\ Perm []
  , "-" /\ Perm [1]
  , "=" /\ Perm [1, 2]
  , "Δ" /\ Spider Black 1 2
  , "." /\ Spider Black 1 0
  , "+" /\ Spider White 2 1
  , "0" /\ Spider White 0 1
  ]

parseContext :: String -> Either String (Context String String)
parseContext = spl "\n" >>> alaF App foldMap toEntry
  where
    spl :: String -> String -> Array String
    spl p = trim >>> split (Pattern p) >>> map trim
    toEntry :: String -> Either String (Context String String)
    toEntry line = case spl ":" line of
      [name, typ] -> case typ # spl "->" <#> (spl " " >>> filter (_ /= "")) of
        [left, right] -> pure $ Map.singleton name (Gen $ Ty left right)
        _ -> do
          permRe <- regex "\\[(.*)\\]" noFlags
          case match permRe typ # map toArray of
            Just [_, Just perm] ->
              pure $ Map.singleton name (Perm $ perm # (spl " " >>> filter (_ /= "") >>> map (readInt 10 >>> floor)))
            _ -> do
              spiderRe <- regex "(\\d+)(o|.)(\\d+)" noFlags
              case match spiderRe typ # map toArray of
                Just [_, Just ls, Just cs, Just rs] -> pure $ Map.singleton name (Spider
                  (if cs == "." then Black else White)
                  (readInt 10 ls # floor)
                  (readInt 10 rs # floor)
                )
                _ -> Left $ "Invalid type: " <> typ
      _ -> Left $ "Invalid signature: " <> line
