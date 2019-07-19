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

import Debug.Trace (spy)

transactionsDictionary :: String \/ Map TxId TxSum
transactionsDictionary = spy "transactionsDictionary" $ map Map.fromFoldable txsPairs'
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
    txsJson = jsonParser txsJsonStr

txsJsonStr :: String
txsJsonStr = """
[
  {
    "status": "ok",
    "hex": "0a20dce4021c15fda2dfd9ec2ef4413b9e5a4ac5cbd8def33c0ca2c071f75a71464b122b0a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b810051a05222bbbb222",
    "hash": "zFsGM27HNS66qmGp1Y1STK48FUA1F12VHLRB51RGWNYWV",
    "decoded": {
      "firing": {
        "execution": "zFsGM26E6xAuYMXox2zMGUChk3HmbEAMGXBiWG3UL7KF5",
        "message": "222bbbb222",
        "path": [5]
      },
      "previous": "zFsGM26F88jGH8HtpdSCBdgRWSVJEWbyDoH1HRRWXTZyC"
    }
  },
  {
    "status": "ok",
    "hex": "0a20dce4021cd649d3a9d1f69832f26739c1d81c873ca5f343ec2dd92d335adfc805122a0a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b810011a04111aaa11",
    "hash": "zFsGM26F88jGH8HtpdSCBdgRWSVJEWbyDoH1HRRWXTZyC",
    "decoded": {
      "firing": {
        "execution": "zFsGM26E6xAuYMXox2zMGUChk3HmbEAMGXBiWG3UL7KF5",
        "message": "111aaa11",
        "path": [1]
      },
      "previous": "zFsGM28DqZKjjGbfCEsjsXTj8xJAqWaBXpDSc1CqR6ihi"
    }
  },
  {
    "status": "ok",
    "hex": "0a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b812240a20dce4021c1447c0b50a5ce982dd4e78650ca3cc642004b4408eac0264da1ca5b81004",
    "hash": "zFsGM28DqZKjjGbfCEsjsXTj8xJAqWaBXpDSc1CqR6ihi",
    "decoded": {
      "firing": {
        "execution": "zFsGM26E6xAuYMXox2zMGUChk3HmbEAMGXBiWG3UL7KF5",
        "path": [4]
      },
      "previous": "zFsGM26E6xAuYMXox2zMGUChk3HmbEAMGXBiWG3UL7KF5"
    }
  },
  {
    "status": "ok",
    "hex": "0a20dce4021cacc5f351d54402799977d7e4f7b86805359aec724805c80ec0b4d546120710001a03aa0003",
    "hash": "zFsGM26E6xAuYMXox2zMGUChk3HmbEAMGXBiWG3UL7KF5",
    "decoded": {
      "firing": { "message": "aa0003", "path": [0] },
      "previous": "zFsGM27o59f9Lu8bWjNHBG7Wbq5iftQA6uDt14zRdjCrH"
    }
  },
  {
    "status": "ok",
    "hex": "0a20dce4021c8f117e89c479665f6d61ff650b150af375d6498b593da6afa8d2ca9f1afa010add010a0a70726976696c656467651001100010021000100210001006100010011000100310001003100010011000100210001004100010031000100510001004100010051000100110001005100010021000100510001006100010021000100610001003100010061000100510001000100310001a036275791a07666f7253616c651a05626c6f636b1a07756e626c6f636b1a047363616e1a086e6f74536f6c64321a0873686f774f7665721a076e6f74536f6c641a066e6f53686f771a04627579271a076275794261636b1a096e6f745265736f6c641a0663726561746512160a046d61696e10011801220a70726976696c656467651800",
    "hash": "zFsGM27o59f9Lu8bWjNHBG7Wbq5iftQA6uDt14zRdjCrH",
    "decoded": {
      "wiring": {
        "nets": [
          {
            "name": "priviledge",
            "partition": [
              1,
              0,
              2,
              0,
              2,
              0,
              6,
              0,
              1,
              0,
              3,
              0,
              3,
              0,
              1,
              0,
              2,
              0,
              4,
              0,
              3,
              0,
              5,
              0,
              4,
              0,
              5,
              0,
              1,
              0,
              5,
              0,
              2,
              0,
              5,
              0,
              6,
              0,
              2,
              0,
              6,
              0,
              3,
              0,
              6,
              0,
              5,
              0,
              0,
              3,
              0
            ],
            "names": [
              "buy",
              "forSale",
              "block",
              "unblock",
              "scan",
              "notSold2",
              "showOver",
              "notSold",
              "noShow",
              "buy'",
              "buyBack",
              "notResold",
              "create"
            ]
          }
        ],
        "diagrams": [
          { "name": "main", "width": 1, "pixels": [1], "names": ["priviledge"] }
        ],
        "labels": [0]
      },
      "previous": "zFsGM27VMNWZne1SSkWnDQTzr6TdjmsKpbxGkJKKaEC8e"
    }
  },
  {
    "status": "ok",
    "hex": "0a0022200a1e47756172616e746565642d456e7472616e63652d546f6b656e2e74657374",
    "hash": "zFsGM27VMNWZne1SSkWnDQTzr6TdjmsKpbxGkJKKaEC8e",
    "decoded": {
      "root": {
        "message": "47756172616e746565642d456e7472616e63652d546f6b656e2e74657374"
      },
      "previous": "z"
    }
  }
]
"""

fromEither :: forall a b. (a -> b) -> Either a b ->  b
fromEither f = either f identity
