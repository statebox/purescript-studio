module Output.Haskell where

import Prelude

import Control.Monad.Free (Free, liftF, wrap)
import Data.Array (intercalate, singleton, replicate, uncons)
import Data.Foldable (foldMap, foldl, length)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.String (toLower, contains)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (mapAccumL)
import Data.Tuple.Nested ((/\))

import Common (foldFix, foldFree, (..<))
import Model

haskellCode :: ∀ ann. Context String String -> Term ann (Brick String) -> String
haskellCode ctx tm = case haskellCode' ctx tm of
  { i, o, code } -> args <> arr (tuple $ foldMap singleton i) (showNested i) `comp` code `comp` arr (showNested o) (tuple $ foldMap singleton o)
  where
    gens = Map.keys $ Map.filter isGen ctx
    args = if length gens > 0 then "\\" <> toLower (intercalate " " gens) <> " -> " else ""


type HaskellCode = { i :: Free Array String, o :: Free Array String, code :: String }

haskellEmpty :: HaskellCode
haskellEmpty = { i: liftF [], o: liftF [], code: "returnA" }

haskellCode' :: ∀ ann. Context String String -> Term ann (Brick String) -> HaskellCode
haskellCode' ctx = foldFix alg where
  alg TUnit = haskellEmpty
  alg (TBox { bid }) = Map.lookup bid ctx # (maybe haskellEmpty $ case _ of
    Perm perm -> perm # foldMapWithIndex (\i p -> ["a" <> show i] /\ ["a" <> show (p - 1)]) #
      \(i /\ o) -> { i: liftF i, o: liftF o, code: arr (tuple i) (tuple o) }
    Spider _ l r -> { i: liftF i, o: liftF o, code: arr (tuple i) out }
      where
        i = 0 ..< l <#> \n -> "i" <> show n
        o = 0 ..< r <#> \n -> "o" <> show n
        out = if l == 1
          then tuple (replicate r "i0")
          else "let o = mconcat [" <> intercalate ", " i <> "] in " <> tuple (replicate r "o")
    Gen (Ty i o) ->
      { i: liftF $ foldMapWithIndex (\j n -> [toLower n <> show j]) i
      , o: liftF $ foldMapWithIndex (\j n -> [toLower n <> show j]) o
      , code: toLower bid }
  )
  alg (TC ts _) = ts # uncons # maybe haskellEmpty \{ head, tail } -> foldl compose head tail
    where
      compose l r = { i: l.i, o: r.o, code : braced $ l.code `comp` arr (showNested l.o) (showNested i') `comp` r.code }
        where
          os = foldMap singleton l.o
          i' = mapAccumL accum os r.i # _.value
          accum os' _ = uncons os' # maybe { accum: [], value: "_" } \{ head, tail } -> { accum: tail, value: head }
  alg (TT ts _) = foldMapWithIndex f ts # g
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
comp "returnA" r = r
comp l "returnA" = l
comp l r = l <> " >>> " <> r

braced :: String -> String
braced s = if contains (Pattern " ") s then "(" <> s <> ")" else s

arr :: String -> String -> String
arr ls rs = if ls == rs then "returnA" else "arr (\\" <> ls <> " -> " <> rs <> ")"

tuple :: Array String -> String
tuple [s] = s
tuple ss = "(" <> intercalate ", " ss <> ")"

showNested :: Free Array String -> String
showNested = foldFree tuple identity
