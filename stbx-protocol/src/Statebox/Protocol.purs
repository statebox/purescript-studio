module Statebox.Protocol where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Statebox.Core.Transaction (FiringTx, HashStr, HashTx, InitialTx, TxId, TxSum(..), WiringTx, evalTxSum, isInitialTx, isUberRootHash)
import Statebox.Protocol.Fire (fire)
import Statebox.Protocol.Store (StoreActions, getTransaction, putTransaction, getExecutionState, updateExecutionState)

type ExecutionId = TxId

data ProcessError
  = NoUberRoot
  | InitialPreviousNoUberRoot             TxId
  | WiringNotPreviousInitial              TxId
  | FiringInitialDuplicateExecution       TxId
  | FiringInitialMissingPrevious          TxId
  | FiringInitialPreviousNotWiring        TxId
  | FiringNormalMissingExecution          TxId ExecutionId
  | FiringNormalPreviousIsNotCurrentState TxId ExecutionId

processTxSum :: HashTx -> StoreActions (Either ProcessError Unit)
processTxSum hashTx = case hashTx.tx of
  UberRootTxInj           -> pure $ Left NoUberRoot
  InitialTxInj  initialTx -> processInitialTx hash.id initialTx
  WiringTxInj   wiringTx  -> processWiringTx  hash.id wiringTx
  FiringTxInj   firingTx  -> processFiringTx  hash.id firingTx

processInitialTx :: HashStr -> InitialTx -> StoreActions (Either ProcessError Unit)
processInitialTx hash initialTx =
  if (isUberRootHash initialTx.previous)
  then map Right $ putTransaction hash $ InitialTxInj initialTx
  else pure $ Left $ InitialPreviousNoUberRoot initialTx.previous

isInitialHash :: TxId -> StoreActions Boolean
isInitialHash hash = do
  maybeTxSum <- getTransaction hash
  case maybeTxSum of
    Nothing -> pure false
    Just tx -> pure $ isInitialTx tx

processWiringTx :: HashStr -> WiringTx -> StoreActions (Either ProcessError Unit)
processWiringTx hash wiringTx =
  let
    previousHash = wiringTx.previous
  in do
    isPreviousInitial <- isInitialHash previousHash
    if isPreviousInitial
    then map Right $ putTransaction hash $ WiringTxInj wiringTx
    else pure $ Left $ WiringNotPreviousInitial hash

processFiringTx :: HashStr -> FiringTx -> StoreActions (Either ProcessError Unit)
processFiringTx hash firingTx =
  -- check if the firing is initial
  case firingTx.firing.execution of
    -- it does not have an execution, hence it is initial
    Nothing        -> processInitialFiringTx hash firingTx
    -- it does have an execution, hence it is a normal firing
    Just execution -> processNormalFiringTx hash firingTx execution

processInitialFiringTx :: HashStr -> FiringTx -> StoreActions (Either ProcessError Unit)
processInitialFiringTx hash firingTx = do
  -- check if execution already exists
  maybeExecution <- getExecutionState hash
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
                putTransaction       hash $ FiringTxInj firingTx
                updateExecutionState hash $ Execution { lastFiring : hash
                                                      , wiring         : firingTx.previous
                                                      }
            )
            (const $ pure $ Left $ FiringInitialPreviousNotWiring hash)
            previous

processNormalFiringTx :: HashStr -> FiringTx -> TxId -> StoreActions (Either ProcessError Unit)
processNormalFiringTx hash firingTx executionHash = do
  maybeExecution <- getExecutionState executionHash
  case maybeExecution of
    -- execution does not exist
    Nothing        -> pure $ Left $ FiringNormalMissingExecution hash executionHash
    -- execution does exist
    Just execution -> do
      -- check if the previous transaction corresponds to the current state of the execution
      if firingTx.previous == execution.lastFiring
      then map Right $ do
        -- fire transition
        putTransaction hash $ FiringTxInj firingTx
        updateExecutionState executionHash { lastFiring: hash
                                           , wiring: execution.wiring
                                           }
      else pure $ Left $ FiringNormalPreviousIsNotCurrentState hash executionHash
