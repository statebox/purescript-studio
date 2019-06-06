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

-- TODO implement Show instance?
data TxSum
  = InitialTxInj InitialTx
  | WiringTxInj WiringTx
  | FiringTxInj FiringTx

evalTxSum
  :: forall a
   . (HashStr  -> a)
  -> (WiringTx -> a)
  -> (FiringTx -> a)
  -> TxSum
  -> a
evalTxSum fi fw ff = case _ of
  InitialTxInj i -> fi i.root.message
  WiringTxInj  w -> fw w
  FiringTxInj  f -> ff f

instance showTxSum :: Show TxSum where
  show = evalTxSum
    (\x -> "(InitialTxInj " <>      x <> ")")
    (\x -> "(WiringTxInj "  <> show x <> ")")
    (\x -> "(FiringTxInj "  <> show x <> ")")

uberRoot_HACK = ""

-- TODO Newer proto versions have a previous hash for initial tx as well.
getPrevious :: TxSum -> HashStr
getPrevious = evalTxSum (\_ -> uberRoot_HACK) (_.previous) (_.previous)
