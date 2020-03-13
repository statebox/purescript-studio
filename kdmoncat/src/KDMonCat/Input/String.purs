module KDMoncat.Input.String where

import Prelude

import Data.Array (filter)
import Data.Array.NonEmpty (toArray)
import Data.Either
import Data.Foldable (foldMap)
import Data.Functor.App (App(..))
import Data.Int (floor)
import Data.Map as Map
import Data.Maybe
import Data.Newtype (alaF)
import Data.String (trim, split)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (noFlags)
import Global (readInt)

import KDMonCat.Model

type Input =
  { pixels :: String
  , context :: String
  }

parsePixels :: String -> Array (Array String)
parsePixels = map (split (Pattern "")) <<< split (Pattern "\n")

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
