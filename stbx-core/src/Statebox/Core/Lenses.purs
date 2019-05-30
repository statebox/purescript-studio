module Statebox.Core.Lenses where

import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Statebox.Core.Transaction (TxSum(..), WiringTx, FiringTx)

_leWiring :: Prism' TxSum WiringTx
_leWiring = prism' WiringTxInj case _ of WiringTxInj x -> Just x
                                         _             -> Nothing

_leFiring :: Prism' TxSum FiringTx
_leFiring = prism' FiringTxInj case _ of FiringTxInj x -> Just x
                                         _             -> Nothing
