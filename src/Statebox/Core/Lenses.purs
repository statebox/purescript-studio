module Statebox.Core.Lenses where

import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Statebox.Core.Transaction (TxSum(..), WiringTx, FiringTx)

_leWiring :: Prism' TxSum WiringTx
_leWiring = prism' LeWiring case _ of LeWiring x -> Just x
                                      _          -> Nothing

_leFiring :: Prism' TxSum FiringTx
_leFiring = prism' LeFiring case _ of LeFiring x -> Just x
                                      _          -> Nothing
