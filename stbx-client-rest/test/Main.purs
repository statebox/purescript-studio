module Test.Main where

import Prelude

import Affjax (URL)
import Control.Coroutine (Consumer, Producer, Process, runProcess, consumer, connect)
import Data.Either (either)
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Aff (Aff, runAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Statebox.Client (requestTransactionsToRoot)
import Statebox.Core.Transaction (HashTx)

apiBaseUrl :: URL
apiBaseUrl = "https://testapi.statebox.io"

main :: Effect Unit
main = do
  void $ runAff (either (log <<< show) (log <<< show)) $ runProcess fetchExampleTransactions

fetchExampleTransactions :: Process Aff Unit
fetchExampleTransactions =
  transactionProducer `connect` transactionConsumer
  where
    transactionProducer :: Producer HashTx Aff Unit
    transactionProducer = requestTransactionsToRoot apiBaseUrl exampleFiringHash1
      where
        exampleFiringHash1 = "zFsGM27HNS66qmGp1Y1STK48FUA1F12VHLRB51RGWNYWV"

    transactionConsumer :: Consumer HashTx Aff Unit
    transactionConsumer = consumer \hashTx -> liftEffect (log $ show hashTx) $> Nothing
