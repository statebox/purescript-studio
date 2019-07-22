module ExampleData where

import Prelude

import Data.Argonaut.Core (Json, toArray)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Profunctor.Choice ((|||))
import Data.Profunctor.Strong ((&&&))
import Data.Either (Either(..))
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (sequence, traverse)
import Statebox.Core.Transaction (TxId, Tx, TxSum(..))
import Statebox.Core.Transaction.Codec (decodeTxWith, decodeTxTxSum, DecodingError(..))
import Statebox.Example.Transactions (transactionsUpTo_zFsGM27HNS66qmGp1Y1STK48FUA1F12VHLRB51RGWNYWV_JsonStr)

import Debug.Trace (spy)

transactionsDictionary :: String \/ Map TxId TxSum
transactionsDictionary = mkTransactionsDictionary transactionsUpTo_zFsGM27HNS66qmGp1Y1STK48FUA1F12VHLRB51RGWNYWV_JsonStr

mkTransactionsDictionary :: String -> String \/ Map TxId TxSum
mkTransactionsDictionary jsonStr =
  spy "transactionsDictionary" $ map Map.fromFoldable txsPairs'
  where
    txsPairs' :: String \/ Array (TxId /\ TxSum)
    txsPairs' = spy "txsPairs'" $ map (map (_.hash &&& _.decoded)) txTxSums''

    txTxSums'' :: String \/ Array (Tx TxSum)
    txTxSums'' = spy "txTxSums''" $ fromEither Left txTxSums'

    txTxSums' :: String \/ (String \/ Array (Tx TxSum))
    txTxSums' = spy "txTxSums'" $ map (traverse decodeTxTxSum) txsJson''

    txsJson'' :: String \/ Array Json
    txsJson'' = Left ||| (maybe (Left "not a JSON Array") Right) $ txsJson'

    txsJson' :: String \/ Maybe (Array Json)
    txsJson' = toArray <$> txsJson

    txsJson :: String \/ Json
    txsJson = jsonParser jsonStr

fromEither :: forall a b. (a -> b) -> Either a b ->  b
fromEither f = either f identity
