module View.Transaction where

import Prelude hiding (div)
import Data.Maybe (maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.HTML (HTML, slot, div, table, tr, td, a, text, p, br, pre)
import Halogen.HTML.Properties (href)

import View.Studio.Model (Action(..))
import View.Studio.Model.Route (WiringFiringInfo)
import Statebox.Client (txUrl)
import Statebox.Core.Transaction (HashStr, FiringTx)

firingTxView :: ∀ s m. MonadAff m => WiringFiringInfo -> FiringTx -> ComponentHTML Action s m
firingTxView wfi firingTx =
  div []
      [ p     [] [ text $ "Firing" ]
      , table [] [ row "service URL"     $ a [ href $ wfi.endpointUrl ]                [ text $ wfi.endpointUrl ]
                 , row "transaction URL" $ a [ href $ txUrl wfi.endpointUrl wfi.hash ] [ text $ txUrl wfi.endpointUrl wfi.hash ]
                 , row "hash"            $ text $ wfi.hash
                 , row "path"            $ text $ show firingTx.firing.path
                 , row "message"         $ text $ maybe "<no message>" show firingTx.firing.message
                 , row "previous"        $ text $ firingTx.previous
                 , row "previous URL"    $ a [ href $ txUrl wfi.endpointUrl firingTx.previous ] [ text $ txUrl wfi.endpointUrl firingTx.previous ]
                 ]
      , br []
      , br []
      , p [] [ pre [] [ text $ show firingTx ] ]
      ]
    where
      row caption content =
        tr [] [ td [] [ text caption ]
              , td [] [ content ]
              ]

