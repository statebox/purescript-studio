module Statebox.Core.Lenses where

import Prelude
import Data.Lens (Lens', Prism', Traversal', lens, prism', _Just)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton, head)
import Statebox.Core.Transaction (TxId, TxSum(..), WiringTx, FiringTx, isExecutionTx)
import Statebox.Core.Types (Firing, Wiring, GluedTransitionIdRaw)

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

_wiring :: Lens' WiringTx Wiring
_wiring = lens (_.wiring) (_ { wiring = _ })

_firing :: Lens' FiringTx Firing
_firing = lens (_.firing) (_ { firing = _ })

_execution :: Lens' Firing (Maybe TxId)
_execution = lens (_.execution) (_ {  execution = _ })

_firingExecution :: Traversal' FiringTx TxId
_firingExecution = _firing <<< _execution <<< _Just

_firingPath :: Lens' Firing GluedTransitionIdRaw
_firingPath = lens (_.path >>> head) (\r x -> r { path = singleton x })
