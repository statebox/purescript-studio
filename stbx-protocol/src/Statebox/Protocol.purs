module Statebox.Protocol where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Statebox.Core.Transaction (FiringTx, HashStr, InitialTx, TxId, TxSum(..), WiringTx, evalTxSum, isUberRootHash)
import Statebox.Protocol.Execution (Execution(..))
import Statebox.Protocol.Fire (fire)
import Statebox.Protocol.Store (StoreActions, getTransaction, putTransaction, getExecution, putExecution)

data ProcessError
  = NoUberRoot
  | InitialPreviousNoUberRoot       TxId
  | WiringNotPreviousInitial        TxId
  | FiringInitialDuplicateExecution TxId
  | FiringInitialMissingPrevious    TxId
  | FiringInitialPreviousNotWiring  TxId

-- | the Hash and the TxSum are two representations of the same transaction
-- | which could turn out to be useful in different places
processTxSum :: HashStr -> TxSum -> StoreActions (Either ProcessError Unit)
processTxSum hash = case _ of
  UberRootTxInj           -> pure $ Left NoUberRoot
  InitialTxInj  initialTx -> processInitialTx hash initialTx
  WiringTxInj   wiringTx  -> processWiringTx  hash wiringTx
  FiringTxInj   firingTx  -> processFiringTx  hash firingTx

processInitialTx :: HashStr -> InitialTx -> StoreActions (Either ProcessError Unit)
processInitialTx hash initialTx =
  if (isUberRootHash initialTx.previous)
  then map Right $ putTransaction hash $ InitialTxInj initialTx
  else pure $ Left $ InitialPreviousNoUberRoot initialTx.previous

isInitialTx :: TxId -> StoreActions Boolean
isInitialTx hash = do
  maybeTxSum <- getTransaction hash
  case maybeTxSum of
    Nothing -> pure false
    Just tx -> pure $ evalTxSum (const false) (const true) (const false) (const false) tx

processWiringTx :: HashStr -> WiringTx -> StoreActions (Either ProcessError Unit)
processWiringTx hash wiringTx =
  let
    previousHash = wiringTx.previous
  in do
    isPreviousInitial <- isInitialTx previousHash
    if isPreviousInitial
    then map Right $ putTransaction hash $ WiringTxInj wiringTx
    else pure $ Left $ WiringNotPreviousInitial hash

processFiringTx :: HashStr -> FiringTx -> StoreActions (Either ProcessError Unit)
processFiringTx hash firingTx =
  -- check if the firing is initial
  case firingTx.firing.execution of
    -- it does not have an execution, hence it is initial
    Nothing -> processInitialFiringTx hash firingTx
    -- it does have an execution, hence it is a normal firing
    Just ex -> ?nf -- processNormalFiringTx hash firingTx ex

processInitialFiringTx :: HashStr -> FiringTx -> StoreActions (Either ProcessError Unit)
processInitialFiringTx hash firingTx = do
  -- check if execution already exists
  maybeExecution <- getExecution hash
  case maybeExecution of
    -- execution already exists
    Just _  -> pure $ Left $ FiringInitialDuplicateExecution hash
    -- execution does not exist yet
    Nothing -> do
      -- check if previous is wiring
      maybePrevious <- getTransaction firingTx.previous
      case maybePrevious of
        -- previous not found
        Nothing       -> pure $ Left $ FiringInitialMissingPrevious hash
        -- previous found
        Just previous -> do
          evalTxSum
            (const $ pure $ Left $ FiringInitialPreviousNotWiring hash)
            (const $ pure $ Left $ FiringInitialPreviousNotWiring hash)
            (\wiringTx ->
              let
                firedTransition = fire firingTx
              in map Right $ do
                putTransaction hash $ FiringTxInj firingTx
                putExecution   hash $ Execution { lastTransition : hash
                                                , wiring         : firingTx.previous
                                                }
            )
            (const $ pure $ Left $ FiringInitialPreviousNotWiring hash)
            previous

-- processNormalFiringTx :: HashStr -> FiringTx -> TxId -> StoreActions (Either ProcessError Unit)
-- processNormalFiringTx hash firingTx executionTxId = ?nf
