module Output.Haskell where

import Prelude

import Control.Monad.Free (Free, liftF, wrap)
import Data.Array (intercalate, singleton, replicate, uncons, unsnoc)
import Data.Foldable (foldMap, foldr, length)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.String (toLower, contains, replaceAll, Pattern(..), Replacement(..))
import Data.Traversable (mapAccumL)
import Data.Tuple.Nested ((/\))

import Common
import Model


haskellCode :: String -> TypedTerm String String -> String
haskellCode fnName tm = case haskellCode' tm of
  { i, o, code } -> typeDecl <> args <> indent ("\n" <> arr (tuple $ foldMap singleton i) (showNested i) `comp` code `comp` arr (showNested o) (tuple $ foldMap singleton o))
  where
    gens = tm # foldMap \{ decl, name } -> case decl of
      Gen ty -> Map.singleton name ty
      otherwise -> Map.empty
    args = if length gens > 0 then fnName <> " " <> toLower (intercalate " " $ Map.keys gens) <> " = " else ""
    typeDecl = fnName <>
      "\n  :: Arrow arr" <>
      "\n  => " <> intercalate "\n  -> " (showTy <$> Map.values gens) <>
      "\n  -> " <> showTy (getAnn tm) <>
      "\n"


type HaskellCode = { i :: Free Array String, o :: Free Array String, code :: String }

haskellEmpty :: HaskellCode
haskellEmpty = { i: liftF [], o: liftF [], code: "returnA" }

haskellCode' :: TypedTerm String String -> HaskellCode
haskellCode' = foldFix \(Ann _ f) -> alg f where
  alg TUnit = haskellEmpty
  alg (TBox { name, decl }) = case decl of
    Perm perm -> perm # foldMapWithIndex (\i p -> ["a" <> show i] /\ ["a" <> show (p - 1)]) #
      \(i /\ o) -> { i: liftF i, o: liftF o, code: arr (tuple i) (tuple o) }
    Spider _ l r -> { i: liftF i, o: liftF o, code: arr (tuple i) out }
      where
        i = 0 ..< l <#> \n -> "i" <> show n
        o = 0 ..< r <#> \n -> "o" <> show n
        out = if l == 1
          then tuple (replicate r "i0")
          else "let o = merge [" <> intercalate ", " i <> "] in " <> tuple (replicate r "o")
    Gen (Ty i o) ->
      { i: liftF $ foldMapWithIndex (\j n -> [toLower n <> show j]) i
      , o: liftF $ foldMapWithIndex (\j n -> [toLower n <> show j]) o
      , code: name }
    Cup -> { i: liftF [], o: liftF ["a0", "a1"], code: "cup" }
    Cap -> { i: liftF ["a0", "a1"], o: liftF [], code: "cap" }
  alg (TC ts) = ts # unsnoc # maybe haskellEmpty (\{ init, last } -> foldr compose last init) # mapCode braced
    where
      compose l r = { i: l.i, o: r.o, code : l.code `comp` arr (showNested l.o) (showNested i') `comp` r.code }
        where
          os = foldMap singleton l.o
          i' = mapAccumL accum os r.i # _.value
          accum os' _ = uncons os' # maybe { accum: [], value: "_" } \{ head, tail } -> { accum: tail, value: head }
  alg (TT ts) = foldMapWithIndex f ts # g
    where
      f j { i, o, code } = [{ i: map (\n -> n <> "_" <> show j) i, o: map (\n -> n <> "_" <> show j) o, code }]
      g l = unsnoc l # maybe haskellEmpty (\{ init, last } -> foldr tensor last init)
      tensor l r =
        { i: wrap [l.i, r.i]
        , o: wrap [l.o, r.o]
        , code: l.code <> " *** " <> r.code
        }

mapCode :: (String -> String) -> HaskellCode -> HaskellCode
mapCode f { i, o, code } = { i, o, code: f code }

comp :: String -> String -> String
comp "returnA" r = r
comp l "returnA" = l
comp l r = l <> "\n>>> " <> r

indent :: String -> String
indent = replaceAll (Pattern "\n") (Replacement "\n  ")

braced :: String -> String
braced s = if contains (Pattern " ") s then "(" <> (if contains (Pattern "\n") s then indent s else s) <> ")" else s

arr :: String -> String -> String
arr ls rs = if ls == rs then "returnA" else "arr (\\" <> ls <> " -> " <> rs <> ")"

tuple :: Array String -> String
tuple [s] = s
tuple ss = "(" <> intercalate ", " ss <> ")"

showNested :: Free Array String -> String
showNested = foldFree tuple identity

showTy :: Ty String -> String
showTy (Ty l r) = "arr " <> tuple l <> " " <> tuple r
