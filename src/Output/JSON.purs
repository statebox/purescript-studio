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
  alg (Ann (Ty l r) (TBox { bid, decl: Gen _ })) = encodeJson
    { type: "generator"
    , name: bid
    , inputTypes: l
    , outputTypes: r }
  alg (Ann (Ty l _) (TBox { bid, decl: Perm p }))
    | take (length p) [1,2,3,4,5,6,7,8,9] == p = encodeJson
      { type: "identity"
      , name: bid
      , typeParams: l }
    | otherwise = encodeJson
      { type: "permutation"
      , name: bid
      , permutation: p
      , typeParams: l }
  alg (Ann (Ty l r) (TBox { bid, decl: Spider c ni no })) = encodeJson
    { type: "spider"
    , name: bid
    , inputs: ni
    , outputs: no
    , color: show c
    , typeParam: l <> r # head
    }
  alg (Ann (Ty l r) (TBox { bid, decl: Cup })) = encodeJson
    { type: "cup"
    , name: bid
    , typeParam: l <> r # head }
  alg (Ann (Ty l r) (TBox { bid, decl: Cap })) = encodeJson
    { type: "cap"
    , name: bid
    , typeParam: l <> r # head }
