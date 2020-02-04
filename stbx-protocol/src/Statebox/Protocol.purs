module Statebox.Protocol where

import Prelude
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Lens ((^?))
import Data.Maybe (Maybe(..), maybe)

import Data.Petrinet.Representation.Marking (emptyMarking)
import Statebox.Core.Lenses (_firingExecution)
import Statebox.Core.Transaction (FiringTx, HashStr, HashTx, InitialTx, TxId, TxSum(..), WiringTx, evalTxSum, isInitialTx, isUberRootHash)
import Statebox.Protocol.Fire (fire)
import Statebox.Protocol.Store (getTransaction, putTransaction, getExecutionState, updateExecutionState) as Store
import Statebox.Protocol.Store (StoreActions)

type ExecutionId = TxId

data ProcessError
  -- | An über-root transaction should exist.
  = NoUberRoot

  -- | The previous transaction of a root (initial) transaction should be an über-root.
  | InitialPreviousShouldBeUberRoot                  TxId

  -- | The previous transaction of a `Wiring` transaction should be a root (initial) transaction.
  | WiringPreviousShouldBeInitial                    TxId

  -- | If an execution already exists, we cannot store it again.
  | FiringInitialShouldBeCreatedOnlyOnce             TxId

  -- | The previous transaction of an initial transaction ('execution transaction') should exist.
  | FiringInitialShouldHavePrevious                  TxId

  -- | The previous transaction of an initial transaction ('execution transaction') should be a `Wiring`.
  | FiringInitialPreviousShouldBeWiring              TxId

  -- | The first transition fired should be initial.
  | FiringInitialTransitionShouldBeInitial           TxId

  -- | A normal firing should refer to an existing execution.
  | FiringNormalShouldHaveExistingExecution          TxId ExecutionId

  -- | The previous transaction of a normal firing should match the current execution state.
  | FiringNormalPreviousShouldMatchCurrentState      TxId ExecutionId

  -- | The execution of a firing should refer to an existing wiring.
  | FiringNormalExecutionShouldPointToExistingWiring TxId ExecutionId

  -- | The wiring of the execution of a normal firing should be a `Wiring`.
  | FiringNormalExecutionWiringShouldBeAWiring       TxId ExecutionId

  -- | The fired transition should be enabled.
  | FiringNormalTransitionShouldBeEnabled            TxId ExecutionId

processTxSum :: HashTx -> StoreActions (ProcessError \/ Unit)
processTxSum hashTx = case hashTx.tx of
  UberRootTxInj           -> pure $ Left NoUberRoot
  InitialTxInj  initialTx -> processInitialTx hashTx.id initialTx
  WiringTxInj   wiringTx  -> processWiringTx  hashTx.id wiringTx
  FiringTxInj   firingTx  -> processFiringTx  hashTx.id firingTx

processInitialTx :: HashStr -> InitialTx -> StoreActions (ProcessError \/ Unit)
processInitialTx hash initialTx =
  if isUberRootHash initialTx.previous
  then map Right $ Store.putTransaction hash $ InitialTxInj initialTx
  else pure $ Left $ InitialPreviousShouldBeUberRoot initialTx.previous

isInitialHash :: TxId -> StoreActions Boolean
isInitialHash hash = do
  maybeTxSum <- Store.getTransaction hash
  case maybeTxSum of
    Nothing -> pure false
    Just tx -> pure $ isInitialTx tx

processWiringTx :: HashStr -> WiringTx -> StoreActions (ProcessError \/ Unit)
processWiringTx hash wiringTx =
  let
    previousHash = wiringTx.previous
  in do
    isPreviousInitial <- isInitialHash previousHash
    if isPreviousInitial
    then map Right $ Store.putTransaction hash $ WiringTxInj wiringTx
    else pure $ Left $ WiringPreviousShouldBeInitial hash

processFiringTx :: HashStr -> FiringTx -> StoreActions (ProcessError \/ Unit)
processFiringTx hash firingTx =
  -- check if the firing is initial
  maybe
    -- it does not have an execution, hence it is initial
    (processInitialFiringTx hash firingTx)
    -- it does have an execution, hence it is a normal firing
    (processNormalFiringTx hash firingTx)
    (firingTx ^? _firingExecution)

processInitialFiringTx :: HashStr -> FiringTx -> StoreActions (ProcessError \/ Unit)
processInitialFiringTx hash firingTx = do
  -- check if execution already exists
  maybeExecution <- Store.getExecutionState hash
  case maybeExecution of
    -- execution already exists
    Just _  -> pure $ Left $ FiringInitialShouldBeCreatedOnlyOnce hash
    -- execution does not exist yet
    Nothing -> do
      -- check if previous is wiring
      maybePrevious <- Store.getTransaction firingTx.previous
      case maybePrevious of
        -- previous not found
        Nothing       -> pure $ Left $ FiringInitialShouldHavePrevious hash
        -- previous found
        Just previous ->
          evalTxSum
            (const $ pure $ Left $ FiringInitialPreviousShouldBeWiring hash)
            (const $ pure $ Left $ FiringInitialPreviousShouldBeWiring hash)
            (\wiringTx -> either
              (const $ pure $ Left $ FiringInitialTransitionShouldBeInitial hash)
              (\newMarking -> map Right $ do
                Store.putTransaction hash $ FiringTxInj firingTx
                Store.updateExecutionState hash $ { lastFiring: hash
                                                  , wiring: firingTx.previous
                                                  , marking: newMarking
                                                  })
              (fire wiringTx.wiring firingTx.firing emptyMarking)
            )
            (const $ pure $ Left $ FiringInitialPreviousShouldBeWiring hash)
            previous

processNormalFiringTx :: HashStr -> FiringTx -> TxId -> StoreActions (ProcessError \/ Unit)
processNormalFiringTx hash firingTx executionHash = do
  maybeExecution <- Store.getExecutionState executionHash
  case maybeExecution of
    -- execution does not exist
    Nothing        -> pure $ Left $ FiringNormalShouldHaveExistingExecution hash executionHash
    -- execution does exist
    Just execution -> do
      -- check if the previous transaction corresponds to the current state of the execution
      if firingTx.previous == execution.lastFiring
      then do
        maybeWiring <- Store.getTransaction execution.wiring
        case maybeWiring of
          Nothing          -> pure $ Left $ FiringNormalExecutionShouldPointToExistingWiring hash executionHash
          Just transaction ->
            evalTxSum
              (const $ pure $ Left $ FiringNormalExecutionWiringShouldBeAWiring hash executionHash)
              (const $ pure $ Left $ FiringNormalExecutionWiringShouldBeAWiring hash executionHash)
              (\wiringTx -> either
                (const $ pure $ Left $ FiringNormalTransitionShouldBeEnabled hash executionHash)
                (\newMarking -> map Right $ do
                  Store.putTransaction hash $ FiringTxInj firingTx
                  Store.updateExecutionState executionHash { lastFiring: hash
                                                          , wiring: execution.wiring
                                                          , marking: newMarking
                                                          })
                (fire wiringTx.wiring firingTx.firing execution.marking))
              (const $ pure $ Left $ FiringNormalExecutionWiringShouldBeAWiring hash executionHash)
              transaction
      else pure $ Left $ FiringNormalPreviousShouldMatchCurrentState hash executionHash
