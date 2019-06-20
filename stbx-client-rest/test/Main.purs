module Test.Main where

import Prelude

import Affjax (URL)
import Control.Coroutine (Consumer, Producer, Process, runProcess, consumer, connect)
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, runAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Statebox.Client (requestTransactionsToRoot)
import Statebox.Core.Transaction (HashStr, TxSum(..), Tx(..), FiringTx, evalTxSum, isUberRootHash)

apiBaseUrl :: URL
apiBaseUrl = "https://testapi.statebox.io"

main :: Effect Unit
main = do
  void $ runAff (either (log <<< show) (log <<< show)) $ runProcess fetchExampleTransactions

fetchExampleTransactions :: Process Aff Unit
fetchExampleTransactions =
  transactionProducer `connect` transactionConsumer
  where
    transactionProducer :: Producer TxSum Aff Unit
    transactionProducer = requestTransactionsToRoot apiBaseUrl exampleFiringHash1
      where
        exampleFiringHash1 = "zFsGM27HNS66qmGp1Y1STK48FUA1F12VHLRB51RGWNYWV"

    transactionConsumer :: Consumer TxSum Aff Unit
    transactionConsumer = consumer \tx -> liftEffect (log $ show tx) $> Nothing
