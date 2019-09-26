module InferType where

import Prelude 

import Control.Monad.Free (Free, liftF, wrap)
import Data.Array (zip, length, uncons, filter, intercalate, singleton)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..), isRight)
import Data.Foldable (foldMap, foldl)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Functor.App (App(..))
import Data.Int (floor)
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (alaF)
import Data.String (trim, split, joinWith, toLower, contains)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (mapAccumL)
import Data.Tuple.Nested (type (/\), (/\))
import Global (readInt)

import Model
import Common


type InferredType bv bid = 
  { type :: Ty (Var bv bid)
  , bounds :: Bounds bv bid
  , matches :: Array (Matches (Var bv bid))
  , errors :: Array String
  }

newtype Bounds bv bid = Bounds (Map (Int /\ Brick bid) (Var bv bid))
instance boundsSemigroup :: (Ord bid) => Semigroup (Bounds bv bid) where
  append (Bounds l) (Bounds r) = let l' = l <#> replace (Bounds r) in Bounds (l' <> (r <#> replace (Bounds l')))
instance boundsMonoid :: (Ord bid) => Monoid (Bounds bv bid) where
  mempty = Bounds (Map.empty)

getType :: ∀ bid bv. Ord bid => InferredType bv bid -> Ty (Var bv bid)
getType { type: Ty l r, bounds } = Ty (l <#> replace bounds) (r <#> replace bounds)

empty :: ∀ bid bv. (Ord bid) => InferredType bv bid
empty = mempty

note :: ∀ bid bv x. (Ord bid) => String -> (x -> InferredType bv bid) -> Maybe x -> InferredType bv bid
note s = maybe (empty { errors = [s] })

showInferred :: ∀ bid bv. Ord bid => Show (Var bv bid) => InferredType bv bid -> String
showInferred it@{ errors } = if length errors == 0 then show (getType it) else joinWith "\n" errors

type HaskellCode = { i :: Free Array String, o :: Free Array String, code :: String }

haskellEmpty :: HaskellCode
haskellEmpty = { i: liftF [], o: liftF [], code: "arr id"}

haskellCode :: ∀ ann. Context String String -> Term ann (Brick String) -> String
haskellCode ctx tm = case haskellCode' ctx tm of
  { i, o, code } -> "\\" <> intercalate " " (Map.keys $ Map.filter isRight ctx) <> " -> " <> arr (liftF $ foldMap singleton i) i `comp` code `comp` arr o (liftF $ foldMap singleton o)

haskellCode' :: ∀ ann. Context String String -> Term ann (Brick String) -> HaskellCode
haskellCode' _ TUnit = haskellEmpty
haskellCode' ctx (TBox { bid }) = Map.lookup bid ctx # (maybe haskellEmpty $ case _ of
    Left perm -> perm # foldMapWithIndex (\i p -> ["a" <> show i] /\ ["a" <> show (p - 1)]) # 
      \(i /\ o) -> { i: liftF i, o: liftF o, code: arr (liftF i) (liftF o) }
    Right (Ty i o) -> 
      { i: liftF $ foldMapWithIndex (\j n -> [toLower n <> show j]) i
      , o: liftF $ foldMapWithIndex (\j n -> [toLower n <> show j]) o
      , code: toLower bid }
  )
haskellCode' ctx (TC ts _) = map (haskellCode' ctx) ts # uncons # maybe haskellEmpty \{ head, tail } -> foldl compose head tail 
  where
    compose l r = { i: l.i, o: r.o, code : braced $ l.code `comp` arr l.o i' `comp` r.code }
      where
        os = foldMap singleton l.o
        i' = mapAccumL accum os r.i # _.value
        accum os' _ = uncons os' # maybe { accum: [], value: "_" } \{ head, tail } -> { accum: tail, value: head }
haskellCode' ctx (TT ts _) = foldMapWithIndex (\i -> f i <<< haskellCode' ctx) ts # g
  where
    f j { i, o, code } = 
      [{ i: map (\n -> n <> "_" <> show j) i
      , o: map (\n -> n <> "_" <> show j) o
      , code
      }]
    g l = uncons l # maybe haskellEmpty \{ head, tail } -> foldl tensor head tail
    tensor :: HaskellCode -> HaskellCode -> HaskellCode
    tensor l r = 
      { i: wrap [l.i, r.i]
      , o: wrap [l.o, r.o]
      , code: "(" <> l.code <> " *** " <> r.code <> ")"
      }

comp :: String -> String -> String
comp "arr id" r = r
comp l "arr id" = l
comp l r = l <> " >>> " <> r

braced :: String -> String
braced s = if contains (Pattern " ") s then "(" <> s <> ")" else s

arr :: Free Array String -> Free Array String -> String
arr l r = if ls == rs then "arr id" else "arr (\\" <> ls <> " -> " <> rs <> ")"
  where
    ls = showNested l
    rs = showNested r

tuple :: Array String -> String
tuple [s] = s
tuple ss = "(" <> intercalate ", " ss <> ")"

showNested :: Free Array String -> String
showNested = foldFree tuple identity


inferType 
  :: ∀ bid ann bv. Ord bid => Show bid => Eq bv => Show (Var bv bid) 
  => Term ann (Brick bid) -> Context bv bid -> InferredType bv bid
inferType TUnit _ = empty
inferType (TBox brick@{ bid }) ctx = 
  Map.lookup bid ctx # note ("Undeclared name: " <> show bid) (inferBoxType brick)
inferType (TT ts _) ctx = foldMap inferType ts ctx
inferType (TC tts _) ctx = uncons tts #
  note "Composition of empty list not supported" \{ head, tail } ->
  foldl compose (inferType head ctx) tail
    where
      compose :: Show (Var bv bid) => InferredType bv bid -> Term ann (Brick bid) -> InferredType bv bid
      compose acc@{ type: Ty a b } term = res
        where
          step@{ type: Ty b' c } = inferType term ctx
          res = if (length b /= length b') 
            then 
              (acc <> step <> empty { matches = [Unmatched Invalid Output b, Unmatched Invalid Input b'] }) { type = Ty a c }
            else let { bounds, matches } = foldMap bindVars (zip b b') in 
              (acc <> step <> empty { matches = [Matched matches], bounds = bounds }) 
                { type = Ty a c }

inferBoxType :: ∀ bid bv. (Ord bid) => Brick bid -> Either (Array Int) (Ty bv) -> InferredType bv bid
inferBoxType brick (Right (Ty i o)) = empty { type = Ty (BoundVar brick <$> i) (BoundVar brick <$> o) }
inferBoxType brick (Left perm) =
  empty { type = Ty (0 ..< length perm <#> (_ + 1) <#> (_ /\ brick) <#> FreeVar) (perm <#> (_ /\ brick) <#> FreeVar) }

bindVars 
  :: ∀ bv bid. Eq bv => Ord bid => Show (Var bv bid)
  => Var bv bid /\ Var bv bid 
  -> { bounds :: Bounds bv bid, matches :: Array (Validity /\ Var bv bid /\ Var bv bid) }
bindVars m@(FreeVar l /\ FreeVar r) = { bounds: Bounds (Map.fromFoldable [ l /\ FreeVar r, r /\ FreeVar l]), matches: [Valid /\ m] }
bindVars m@(l /\ FreeVar r)         = { bounds: Bounds (Map.singleton r l), matches: [Valid /\ m] }
bindVars m@(FreeVar l /\ r)         = { bounds: Bounds (Map.singleton l r), matches: [Valid /\ m] }
bindVars m@(l /\ r) | l == r        = { bounds: mempty, matches: [Valid /\ m] }
                    | otherwise     = { bounds: mempty, matches: [Invalid /\ m] }

replace :: ∀ bv bid. Ord bid => Bounds bv bid -> Var bv bid -> Var bv bid
replace _ (BoundVar box bv) = BoundVar box bv
replace (Bounds bounds) (FreeVar fv) = Map.lookup fv bounds # fromMaybe (FreeVar fv)


defaultEnv :: Context String String
defaultEnv = Map.fromFoldable
  [ "0" /\ Left []
  , " " /\ Left []
  , "-" /\ Left [1]
  , "=" /\ Left [1, 2]
  ]

parseContext :: String -> Either String (Context String String)
parseContext = spl "\n" >>> alaF App foldMap toEntry
  where
    spl :: String -> String -> Array String
    spl p = trim >>> split (Pattern p) >>> map trim
    toEntry :: String -> Either String (Context String String)
    toEntry line = case spl ":" line of 
      [name, typ] -> case typ # spl "->" <#> (spl " " >>> filter (_ /= "")) of
        [left, right] -> pure $ Map.singleton name (Right $ Ty left right)
        _ -> do
          re <- regex "\\[(.*)\\]" noFlags
          case match re typ # map toArray of 
            Just [_, Just perm] -> 
              pure $ Map.singleton name (Left $ perm # (spl " " >>> filter (_ /= "") >>> map (readInt 10 >>> floor)))
            _ -> Left $ "Invalid type: " <> typ
      _ -> Left $ "Invalid signature: " <> line
