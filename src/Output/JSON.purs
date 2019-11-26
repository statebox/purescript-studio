module Output.JSON where

import Prelude

import Data.Array (take, length, head)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)

import Common
import Model

json :: TypedTerm String String -> String
json = foldFix alg >>> stringify where
  alg (Ann _ TUnit) = encodeJson { type: "unit" }
  alg (Ann _ (TC terms)) = encodeJson { type: "compose", terms }
  alg (Ann _ (TT terms)) = encodeJson { type: "tensor", terms }
  alg (Ann (Ty l r) (TBox { name, decl: Gen _ })) = encodeJson
    { type: "generator"
    , name
    , inputTypes: l
    , outputTypes: r }
  alg (Ann (Ty l _) (TBox { name, decl: Perm p }))
    | take (length p) [1,2,3,4,5,6,7,8,9] == p = encodeJson
      { type: "identity"
      , name
      , typeParams: l }
    | otherwise = encodeJson
      { type: "permutation"
      , name
      , permutation: p
      , typeParams: l }
  alg (Ann (Ty l r) (TBox { name, decl: Spider c ni no })) = encodeJson
    { type: "spider"
    , name
    , inputs: ni
    , outputs: no
    , color: show c
    , typeParam: l <> r # head
    }
  alg (Ann (Ty l r) (TBox { name, decl: Cup })) = encodeJson
    { type: "cup"
    , name
    , typeParam: l <> r # head }
  alg (Ann (Ty l r) (TBox { name, decl: Cap })) = encodeJson
    { type: "cap"
    , name
    , typeParam: l <> r # head }
