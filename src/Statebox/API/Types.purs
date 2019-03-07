module Statebox.API.Types where

type HexStr = String

type HashStr = HexStr

type URL = String

type TxId = HexStr

type StatusStr = String

type PathElem = Int

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

type Wiring =
  { nets     :: Array Net
  , diagrams :: Array Diagram
  , labels   :: Array Int
  }

type Net =
  { name      :: String
  , partition :: Array Int
  , names     :: Array String
  }

type Diagram =
  { name   :: String
  , width  :: Int
  , pixels :: Array Int
  , names  :: Array String
  }

--------------------------------------------------------------------------------

type FiringTx =
  { firing   :: Firing
  , previous :: TxId
  }

type Firing =
  { execution :: HexStr
  , path      :: Array PathElem
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
