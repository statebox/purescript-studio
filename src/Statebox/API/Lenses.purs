module Statebox.API.Lenses where

import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Statebox.API.Types

_leWiring :: Prism' TxSum WiringTx
_leWiring = prism' LeWiring case _ of LeWiring x -> Just x
                                      _          -> Nothing
