module Output.JSON where

import Prelude

import Data.Array (take, length, head)
import Data.Argonaut.Core hiding (toNumber)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Foreign.Object
import Type.Row.Homogeneous

import Common
import Model

json :: TypedTerm String String -> String
json = foldFix alg >>> stringify where
  alg (Ann _ TUnit) = o { type: fromString "unit" }
  alg (Ann _ (TC terms)) = o { type: fromString "compose", terms: fromArray terms }
  alg (Ann _ (TT terms)) = o { type: fromString "tensor", terms: fromArray terms }
  alg (Ann (Ty l r) (TBox { bid, decl: Gen _ })) =
    o { type: fromString "generator"
      , name: fromString bid
      , inputTypes: fromArray (map fromString l)
      , outputTypes: fromArray (map fromString r) }
  alg (Ann (Ty l _) (TBox { bid, decl: Perm p }))
    | take (length p) [1,2,3,4,5,6,7,8,9] == p =
      o { type: fromString "identity"
        , name: fromString bid
        , typeParams: fromArray (map fromString l) }
    | otherwise =
      o { type: fromString "permutation"
        , name: fromString bid
        , permutation: fromArray (map n p)
        , typeParams: fromArray (map fromString l) }
  alg (Ann (Ty l r) (TBox { bid, decl: Spider c ni no })) =
    o { type: fromString "spider"
      , name: fromString bid
      , inputs: n ni
      , outputs: n no
      , color: fromString (show c)
      , typeParam: l <> r # head # maybe jsonNull fromString
      }

o :: ∀ r. Homogeneous r Json => Record r -> Json
o = fromHomogeneous >>> fromObject

n :: Int -> Json
n = toNumber >>> fromNumber
