module Statebox.Core.Transaction where

import Prelude
import Data.Maybe (Maybe(..))
import Statebox.Core.Types (Initial, Firing, Wiring, HexStr)

type HashStr = HexStr

type TxId = HexStr

type StatusStr = String

type Tx a =
  { status  :: StatusStr
  , hash    :: TxId
  , hex     :: String
  , decoded :: a
  }

mapTx :: ∀ a b. (a -> b) -> Tx a -> Tx b
mapTx f x = x { decoded = f x.decoded }

-- TODO in newer versions of the protocol, transactions have a field 'hash', which here is called 'id' so those are two names for the same thing
-- TODO in newer versions of the protocol, transactions have a field 'hash', which here is called 'id' so those are two names for the same thing
-- TODO in newer versions of the protocol, transactions have a field 'hash', which here is called 'id' so those are two names for the same thing
type HashTx = { id :: TxId, tx :: TxSum }

attachTxId :: TxId -> TxSum -> HashTx
attachTxId id tx = {id, tx}

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

-- | The transaction types used in the Statebox protocol.
data TxSum

  -- | The 'über-root' is a 'virtual' transaction that terminates the transaction chain in the `previous` direction.
  -- | It has a hash code (see `uberRootHash`) that identifies it, but it has no body and is never actually sent,
  -- | hence the 'virtual'.
  = UberRootTxInj

  -- | The initial root namespace transaction.
  | InitialTxInj InitialTx

  -- | This describes the protocol net in terms diagrams of open Petri Nets.
  | WiringTxInj WiringTx

  -- | This represents a firing of the protocol net, as part of the current execution chain. The history of this
  -- | is found by traversing the chain along its `previous` transactions.
  | FiringTxInj FiringTx

evalTxSum
  :: forall a
   . (Unit -> a)
  -> (InitialTx -> a)
  -> (WiringTx -> a)
  -> (FiringTx -> a)
  -> TxSum
  -> a
evalTxSum fu fi fw ff = case _ of
  UberRootTxInj  -> fu unit
  InitialTxInj i -> fi i
  WiringTxInj  w -> fw w
  FiringTxInj  f -> ff f

instance showTxSum :: Show TxSum where
  show = evalTxSum
    (\x -> "(UberRootTxInj " <> show x <> ")")
    (\x -> "(InitialTxInj "  <> show x <> ")")
    (\x -> "(WiringTxInj "   <> show x <> ")")
    (\x -> "(FiringTxInj "   <> show x <> ")")

-- | `InitialTx` ('root') transactions are children of the virtual 'über-root', indicated by this hash.
uberRootHash :: TxId
uberRootHash = "z"

isUberRootHash :: TxId -> Boolean
isUberRootHash hash = hash == uberRootHash

getPrevious :: TxSum -> Maybe TxId
getPrevious = evalTxSum (const Nothing) (Just <<< _.previous) (Just <<< _.previous) (Just <<< _.previous)
