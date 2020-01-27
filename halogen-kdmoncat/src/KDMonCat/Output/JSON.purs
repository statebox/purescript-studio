module KDMonCat.Output.JSON where

import Prelude

import Data.Array (take, length, head)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)

import KDMonCat.Common
import KDMonCat.Model

json :: TypedTerm String String -> String
json = foldFix alg >>> stringify where
  alg (Ann (Ty l r) TUnit) = encodeJson
    { type: "unit"
    , inputTypes: l
    , outputTypes: r }
  alg (Ann (Ty l r) (TC terms)) = encodeJson
    { type: "compose"
    , terms
    , inputTypes: l
    , outputTypes: r }
  alg (Ann (Ty l r) (TT terms)) = encodeJson
    { type: "tensor"
    , terms
    , inputTypes: l
    , outputTypes: r }
  alg (Ann (Ty l r) (TBox { name, decl: Gen _ })) = encodeJson
    { type: "generator"
    , name
    , inputTypes: l
    , outputTypes: r }
  alg (Ann (Ty l r) (TBox { name, decl: Perm p }))
    | take (length p) [1,2,3,4,5,6,7,8,9] == p = encodeJson
      { type: "identity"
      , name
      , typeParams: l
      , inputTypes: l
      , outputTypes: r }
    | otherwise = encodeJson
      { type: "permutation"
      , name
      , permutation: p
      , typeParams: l
      , inputTypes: l
      , outputTypes: r }
  alg (Ann (Ty l r) (TBox { name, decl: Spider c ni no })) = encodeJson
    { type: "spider"
    , name
    , inputs: ni
    , outputs: no
    , color: show c
    , typeParam: l <> r # head
    , inputTypes: l
    , outputTypes: r
    }
  alg (Ann (Ty l r) (TBox { name, decl: Cup })) = encodeJson
    { type: "cup"
    , name
    , typeParam: l <> r # head
    , inputTypes: l
    , outputTypes: r }
  alg (Ann (Ty l r) (TBox { name, decl: Cap })) = encodeJson
    { type: "cap"
    , name
    , typeParam: l <> r # head
    , inputTypes: l
    , outputTypes: r }
