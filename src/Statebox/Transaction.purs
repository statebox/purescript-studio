module Statebox.Transaction where

import Statebox.API.Types

type HashStr = HexStr

type TxId = HexStr

type StatusStr = String

type Tx a =
  { status  :: StatusStr
  , hex     :: String
  , decoded :: a
  }

--------------------------------------------------------------------------------

type WiringTx =
  { wiring   :: Wiring
  , previous :: TxId
  }

type FiringTx =
  { firing   :: Firing
  , previous :: TxId
  }

--------------------------------------------------------------------------------

data TxSum
  = LeInitial HashStr
  | LeWiring WiringTx
  | LeFiring FiringTx

namespaceRootHash_HACK = "deadbeef"

uberRoot_HACK = ""

-- TODO Newer proto versions have a previous hash for initial tx as well.
getPrevious :: TxSum -> HashStr
getPrevious val = case val of
  LeWiring  w -> w.previous
  LeFiring  f -> f.previous
  LeInitial i -> uberRoot_HACK
