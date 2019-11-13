module Statebox.Service where

import Prelude
import Statebox.Core.Types (HexStr)
import Statebox.Core.Transaction (HashStr)

-- | Based on the `StateboxException`s thrown in https://github.com/statebox/cloud/blob/73158c3a779cbc8a6348aac60e2d0b21e907b2c1/services/tx/process-tx.js.
data Err
  -- | TODO Align with js implementation.
  = TxNotFound { hash :: HashStr }

  -- | Additional error data to store: none.
  | TxNotHex { txHex :: HexStr }

  -- | Additional error data to store: none.
  | TxNoTxField

  -- | Additional error data to store: `{error: e}`.
  | TxDecodeFail { txHex :: HexStr }

  -- | Additional error data to store: none.
  | RootNonexistPrev { previous :: HashStr }

  -- | Additional error data to store: `{stateHash, executionId, enabled}`.
  | InitExecExists

  -- | Additional error data to store: none.
  | InitNonexistPrev { previous :: HashStr }

  -- | Additional error data to store: `{previous, stateHash}`.
  | InvalidState

  -- | Additional error data to store: `{enabled: en0, transition: tr}`.
  | TxNotEnabled

toMessage :: Err -> String
toMessage = case _ of
  TxNotFound       i -> "Transaction " <> i.hash <> " not found."
  TxNotHex         i -> "Transaction " <> i.txHex <> " is not a hexadecimal string."
  TxNoTxField        -> "You must pass a JSON body with a 'tx' attribute containing a hex-encoded Statebox transaction."
  TxDecodeFail     i -> "Failed to decode transaction " <> i.txHex <> "."
  RootNonexistPrev i -> "Root transaction must have 'previous' attribute set to 'z', yours has " <> i.previous <> "."
  InitExecExists     -> "Execution already exists."
  InitNonexistPrev i -> "Failed to fire, 'previous' hash " <> i.previous <> " could not be found."
  InvalidState       -> "Invalid state."
  TxNotEnabled       -> "Transition not enabled."

errorCode :: Err -> String
errorCode = case _ of
  TxNotFound       _ -> "tx-not-found" -- TODO confirm
  TxNotHex         _ -> "tx-not-hex"
  TxNoTxField        -> "tx-no-tx"
  TxDecodeFail     _ -> "tx-decode-fail"
  RootNonexistPrev _ -> "root-nonexist-prev"
  InitExecExists     -> "init-exec-exists"
  InitNonexistPrev _ -> "init-nonexist-prev"
  InvalidState       -> "invalid-state"
  TxNotEnabled       -> "tx-not-enabled"

instance showErr :: Show Err where
  show = show <<< errorCode
