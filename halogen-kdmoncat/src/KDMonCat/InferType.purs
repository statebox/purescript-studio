module KDMonCat.InferType where

import Prelude

import Data.Array (zip, uncons, filter, slice, (!!), head, last)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, foldl, fold, length)
import Data.Functor.App (App(..))
import Data.Int (floor)
import Data.List (List(..))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (alaF)
import Data.String (trim, split, joinWith)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Global (readInt)

import KDMonCat.Model
import KDMonCat.Common


type Meta bv =
  ( bounds :: Bounds bv
  , matches :: Array (Matches (VarWithBox bv))
  , errors :: Array String
  )
type InferredType bv = { type :: Ty (VarWithBox bv) | Meta bv }

newtype Bounds bv = Bounds (Map (Int /\ Box) (Var bv))
instance boundsSemigroup :: FlipDir bv => Semigroup (Bounds bv) where
  append (Bounds l) (Bounds r) = let l' = l <#> replace (Bounds r) in Bounds (l' <> (r <#> replace (Bounds l')))
instance boundsMonoid :: FlipDir bv => Monoid (Bounds bv) where
  mempty = Bounds (Map.empty)


getType :: ∀ bv. Ty (VarWithBox bv) -> Ty (Var bv)
getType (Ty l r) = Ty (l <#> _.var) (r <#> _.var)

empty :: ∀ bv. FlipDir bv => InferredType bv
empty = mempty

note :: ∀ bv x. FlipDir bv => String -> (x -> InferredType bv) -> Maybe x -> InferredType bv
note s = maybe (empty { errors = [s] })

showInferred :: ∀ bv b r. Show (Var bv) => { term :: TypedTerm bv b, errors :: Array String | r } -> String
showInferred { term, errors } = if length errors == 0 then show (getAnn term) else joinWith "\n" errors


inferType
  :: ∀ bid ann bv. Ord bid => Show bid => Eq bv => Show (Var bv) => FlipDir bv
  => Context bv bid -> Term ann (Brick bid) -> { term :: TypedTerm bv bid | Meta bv }
inferType ctx tm = { term, bounds, matches: matches <> typeToMatches, errors }
  where
  alg TUnit = empty
  alg (TBox { box, decl }) = inferBoxType box decl
  alg (TT ts) = fold ts
  alg (TC tts) = uncons tts #
    note "Composition of empty list not supported" \{ head, tail } ->
    foldl compose head tail
      where
        compose :: InferredType bv -> InferredType bv -> InferredType bv
        compose acc@{ type: Ty a b } step@{ type: Ty b' c } = res
          where
            res = if ((length b :: Int) /= length b')
              then
                (acc <> step <> empty { matches = [Unmatched Invalid Output b, Unmatched Invalid Input b'] }) { type = Ty a c }
              else let { bounds, matches } = foldl bindVars mempty (zip b b') in
                replaceInferredType bounds $
                  (acc <> step <> empty { bounds = bounds, matches = [Matched matches] })
                    { type = Ty (a <#> replaceBoxed bounds) (c <#> replaceBoxed bounds) }
  tmWithDecl = tm # traverse (\{ bid, box } -> Map.lookup bid ctx # maybe (Left $ "Undeclared name: " <> show bid) \decl -> Right { bid, box, decl: decl.type, name: decl.name })
  fatTerm = tmWithDecl # either (const (Fix (Ann empty TUnit))) (reannotateFix alg)
  typeToMatches = case (getAnn fatTerm).type of Ty l r -> [Unmatched Valid Input l, Unmatched Valid Output r]
  { bounds, matches, errors } = getAnn fatTerm
  term = fatTerm # mapAccumAnn (\s it -> it # replaceInferredType bounds # _.type # map _.var # varsToString s) mempty # _.value

inferBoxType :: ∀ bv. FlipDir bv => Box -> TypeDecl bv -> InferredType bv
inferBoxType box (Gen (Ty i o)) = empty { type = Ty (i <#> \bv -> { box, var: BoundVar bv }) (o <#> \bv -> { box, var: BoundVar bv }) }
inferBoxType box (Perm perm) =
  empty { type = Ty (0 ..< length perm <#> (_ + 1) <#> \i -> { box, var: FreeVar false (i /\ box) }) (perm <#> \i -> { box, var: FreeVar false (i /\ box) }) }
inferBoxType box (Spider _ l r) =
  empty { type = Ty (0 ..< l <#> const { box, var: FreeVar false (0 /\ box) }) (0 ..< r <#> const { box, var: FreeVar false (0 /\ box) }) }
inferBoxType box Cup =
  empty { type = Ty [] [{ box, var: FreeVar true (0 /\ box) }, { box, var: FreeVar false (0 /\ box) }] }
inferBoxType box Cap =
  empty { type = Ty [{ box, var: FreeVar false (0 /\ box) }, { box, var: FreeVar true (0 /\ box) }] [] }

bindVars
  :: ∀ bv. Eq bv => FlipDir bv
  => { bounds :: Bounds bv, matches :: Array (Validity /\ VarWithBox bv /\ VarWithBox bv) }
  -> VarWithBox bv /\ VarWithBox bv
  -> { bounds :: Bounds bv, matches :: Array (Validity /\ VarWithBox bv /\ VarWithBox bv) }
bindVars bm@{ bounds, matches } (l /\ r) = bm <> bindVars' (replaceBoxed bounds l /\ replaceBoxed bounds r)

bindVars'
  :: ∀ bv. Eq bv => FlipDir bv
  => VarWithBox bv /\ VarWithBox bv
  -> { bounds :: Bounds bv, matches :: Array (Validity /\ VarWithBox bv /\ VarWithBox bv) }
bindVars' (ml@{ var: l } /\ { box, var: FreeVar b r })   = { bounds: Bounds (Map.singleton r (flipDirection b l)), matches: [Valid /\ ml /\ { box, var: l }] }
bindVars' ({ box, var: FreeVar b l } /\ mr@{ var: r })   = { bounds: Bounds (Map.singleton l (flipDirection b r)), matches: [Valid /\ { box, var: r } /\ mr] }
bindVars' m@({ var: l } /\ { var: r }) | l == r        = { bounds: mempty, matches: [Valid /\ m] }
                                       | otherwise     = { bounds: mempty, matches: [Invalid /\ m] }


replaceInferredType :: ∀ bv. FlipDir bv => Bounds bv -> InferredType bv -> InferredType bv
replaceInferredType bounds it@{ type: Ty l r, matches } = it
  { type = Ty (l <#> replaceBoxed bounds) (r <#> replaceBoxed bounds)
  , matches = replaceMatches bounds <$> matches
  }

replaceMatches :: ∀ bv. FlipDir bv => Bounds bv -> Matches (VarWithBox bv) -> Matches (VarWithBox bv)
replaceMatches bounds (Matched ms) = Matched (ms <#> replaceMatched bounds)
replaceMatches bounds (Unmatched v io vs) = Unmatched v io (vs <#> replaceBoxed bounds)

replaceMatched :: ∀ bv. FlipDir bv => Bounds bv -> Validity /\ VarWithBox bv /\ VarWithBox bv -> Validity /\ VarWithBox bv /\ VarWithBox bv
replaceMatched bounds (val /\ l /\ r) = val /\ replaceBoxed bounds l /\ replaceBoxed bounds r

replaceBoxed :: ∀ bv. FlipDir bv => Bounds bv -> VarWithBox bv -> VarWithBox bv
replaceBoxed bounds { box, var } = { box, var: replace bounds var }

replace :: ∀ bv. FlipDir bv => Bounds bv -> Var bv -> Var bv
replace _ (BoundVar bv) = BoundVar bv
replace (Bounds bounds) (FreeVar b fv) = Map.lookup fv bounds # maybe (FreeVar b fv) (flipDirection b)


getSubTerm :: ∀ brick bv. Selection -> TypedTerm bv brick -> TypedTerm bv brick
getSubTerm s (Fix (Ann _ (TT ts))) = getSubTerm' s ts \ts' -> Fix (Ann (foldMap getAnn ts') (TT ts'))
getSubTerm s (Fix (Ann _ (TC ts))) = getSubTerm' s ts \ts' -> case head ts', last ts' of
  Just (Fix (Ann (Ty l _) _)), Just (Fix (Ann (Ty _ r) _)) -> Fix (Ann (Ty l r) (TC ts'))
  _, _ -> Fix (Ann mempty (TC []))
getSubTerm _ t = t

getSubTerm'
  :: ∀ brick bv. Selection -> Array (TypedTerm bv brick)
  -> (Array (TypedTerm bv brick) -> TypedTerm bv brick) -> TypedTerm bv brick
getSubTerm' { path: Cons i Nil, count } ts mkTerm = mkTerm (slice i (i + count) ts)
getSubTerm' { path: Cons i path, count } ts mkTerm = maybe (mkTerm []) (getSubTerm { path, count }) (ts !! i)
getSubTerm' _ _ mkTerm = mkTerm []


defaultEnv :: Context String String
defaultEnv = Map.fromFoldable $ map (\(name /\ ty) -> name /\ { name, type: ty })
  [ " " /\ Perm []
  , "-" /\ Perm [1]
  , "=" /\ Perm [1, 2]
  , "σ" /\ Perm [2, 1]
  , "Δ" /\ Spider Black 1 2
  , "δ" /\ Spider Black 1 2
  , "." /\ Spider Black 1 0
  , "ε" /\ Spider Black 1 0
  , "+" /\ Spider White 2 1
  , "μ" /\ Spider White 2 1
  , "0" /\ Spider White 0 1
  , "η" /\ Spider White 0 1
  , "(" /\ Cup
  , ")" /\ Cap
  ]

parseContext :: String -> Either String (Context String String)
parseContext = spl "\n" >>> alaF App foldMap toEntry
  where
    spl :: String -> String -> Array String
    spl p = trim >>> split (Pattern p) >>> map trim
    parseName :: String -> { name :: String, bids :: Array String }
    parseName nameBid = case nameBid # spl "@" of
      [name, bids] -> { name, bids: toCharArray bids <#> singleton }
      _ -> { name: nameBid, bids: toCharArray nameBid <#> singleton }
    toEntry :: String -> Either String (Context String String)
    toEntry line = case spl ":" line of
      [nameBid, typ] -> let { name, bids } = parseName nameBid in
        (\ty -> foldMap (\bid -> Map.singleton bid { name: if name == "" then bid else name, type: ty }) bids) <$>
        case typ # spl "->" <#> (spl " " >>> filter (_ /= "")) of
          [left, right] -> pure $ Gen $ Ty left right
          _ -> do
            permRe <- regex "^\\[(.*)\\]$" noFlags
            case match permRe typ # map toArray of
              Just [_, Just perm] ->
                pure (Perm $ perm # (spl " " >>> filter (_ /= "") >>> map (readInt 10 >>> floor)))
              _ -> do
                spiderRe <- regex "^(\\d+)(o|\\.)(\\d+)$" noFlags
                case match spiderRe typ # map toArray of
                  Just [_, Just ls, Just cs, Just rs] -> pure (Spider
                    (if cs == "." then Black else White)
                    (readInt 10 ls # floor)
                    (readInt 10 rs # floor)
                  )
                  _ -> Left $ "Invalid type: " <> typ
      _ -> Left $ "Invalid signature: " <> line
