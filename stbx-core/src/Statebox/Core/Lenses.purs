module Statebox.Core.Lenses where

import Prelude
import Data.Lens (Prism', prism', Lens', lens)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton, head)
import Statebox.Core.Transaction (TxSum(..), WiringTx, FiringTx, isExecutionTx)
import Statebox.Core.Types (Firing, GluedTransitionIdRaw)

--------------------------------------------------------------------------------

_wiringTx :: Prism' TxSum WiringTx
_wiringTx = prism' WiringTxInj case _ of WiringTxInj x -> Just x
                                         _             -> Nothing

_firingTx :: Prism' TxSum FiringTx
_firingTx = prism' FiringTxInj case _ of FiringTxInj x -> Just x
                                         _             -> Nothing

_executionTx :: Prism' TxSum FiringTx
_executionTx = prism' FiringTxInj $ case _ of FiringTxInj x | isExecutionTx x -> Just x
                                              _                               -> Nothing

--------------------------------------------------------------------------------

_firing :: Lens' FiringTx Firing
_firing = lens (_.firing) (_ { firing = _ })

_firingPath :: Lens' Firing GluedTransitionIdRaw
_firingPath = lens (_.path >>> head) (\r x -> r { path = singleton x })
