module Statebox.Core.Transaction where

import Prelude
import Statebox.Core.Types (Initial, Firing, Wiring, HexStr)

type HashStr = HexStr

type TxId = HexStr

type StatusStr = String

type Tx a =
  { status  :: StatusStr
  , hex     :: String
  , decoded :: a
  }

--------------------------------------------------------------------------------

type InitialTx =
  { root     :: Initial
  , previous :: TxId
  }

type WiringTx =
  { wiring   :: Wiring
  , previous :: TxId
  }

type FiringTx =
  { firing   :: Firing
  , previous :: TxId
  }

--------------------------------------------------------------------------------

-- | There are 3 types of transaction, and an additional 'virtual' transaction called
-- | the 'über-root', which terminates the transaction chain in the `previous` direction.
-- | This über-root has a hash code (see `uberRootHash`) that identifies it, but it has
-- | no body and is never actually sent, hence the 'virtual'.
data TxSum
  = InitialTxInj InitialTx
  | WiringTxInj WiringTx
  | FiringTxInj FiringTx

evalTxSum
  :: forall a
   . (InitialTx -> a)
  -> (WiringTx -> a)
  -> (FiringTx -> a)
  -> TxSum
  -> a
evalTxSum fi fw ff = case _ of
  InitialTxInj i -> fi i
  WiringTxInj  w -> fw w
  FiringTxInj  f -> ff f

instance showTxSum :: Show TxSum where
  show = evalTxSum
    (\x -> "(InitialTxInj " <> show x <> ")")
    (\x -> "(WiringTxInj "  <> show x <> ")")
    (\x -> "(FiringTxInj "  <> show x <> ")")

-- | `InitialTx` ('root') transactions are children of the virtual 'über-root', indicated by this hash.
uberRootHash :: HashStr
uberRootHash = "z"

getPrevious :: TxSum -> HashStr
getPrevious = evalTxSum (_.previous) (_.previous) (_.previous)
