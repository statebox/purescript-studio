module Statebox.Core.Lenses where

import Prelude
import Data.Lens (Prism', Iso', Lens', lens, prism', re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton, head)
import Statebox.Core.Transaction (TxSum(..), WiringTx, FiringTx, isExecutionTx)
import Statebox.Core.Types (Firing, GluedTransitionId(..), GluedTransitionIdRaw)

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

_firingPath :: Lens' Firing GluedTransitionId
_firingPath = _firingPathRaw <<< re _GluedTransitionId

_firingPathRaw :: Lens' Firing GluedTransitionIdRaw
_firingPathRaw = lens (_.path >>> head) (\r x -> r { path = singleton x })

_GluedTransitionId :: Iso' GluedTransitionId GluedTransitionIdRaw
_GluedTransitionId = _Newtype
