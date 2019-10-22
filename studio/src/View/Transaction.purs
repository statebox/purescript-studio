module View.Transaction where

import Prelude hiding (div)
import Affjax (URL) -- TODO eliminate
import Data.Array (mapMaybe)
import Data.Lens (over, preview, _Just, second)
import Data.Maybe (Maybe, maybe)
import Data.Either.Nested (type (\/))
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.HTML (HTML, slot, div, table, tr, td, a, text, p, br, pre)
import Halogen.HTML.Properties (href)

import View.Studio.Model (Action(..))
import View.Studio.Model.Route (WiringFiringInfo, ExecutionTrace)
import Statebox.Client (txUrl)
import Statebox.Core.Transaction (HashStr, TxSum, FiringTx, WiringTx, isExecution)
import Statebox.Core.Lenses (_firingTx, _firing, _firingPath)


firingTxView :: ∀ s m. MonadAff m => WiringFiringInfo -> FiringTx -> String \/ ExecutionTrace -> ComponentHTML Action s m
firingTxView wfi tx executionTrace =
  div []
      [ p     [] [ text $ if isExecution tx.firing then "Execution" else "Firing" ]
      , table [] $ txWrapperRows wfi tx <>
                   firingTxBodyRows wfi tx <>
                   [ row "trace" $ text $ show $ map (mapMaybe (preview (second <<< _Just <<< _firingTx <<< _firing <<< _firingPath))) executionTrace ] <>
                   [ row "trace raw" $ text (show executionTrace) ]
      ]

wiringTxView :: ∀ s m. MonadAff m => WiringFiringInfo -> WiringTx -> ComponentHTML Action s m
wiringTxView wfi tx =
  div []
      [ p     [] [ text $ "Wiring" ]
      , table [] $ txWrapperRows wfi tx <> wiringTxBodyRows wfi tx
      ]

--------------------------------------------------------------------------------

-- | Some info common to (pretty much) all transaction types
txWrapperRows :: ∀ r s m. MonadAff m => WiringFiringInfo -> { previous :: HashStr | r } -> Array (ComponentHTML Action s m)
txWrapperRows wfi tx =
  [ row "service URL"     $ a [ href $ wfi.endpointUrl ]                [ text $ wfi.endpointUrl ]
  , row "transaction URL" $ a [ href $ txUrl wfi.endpointUrl wfi.hash ] [ text $ txUrl wfi.endpointUrl wfi.hash ]
  , row "hash"            $ text $ wfi.hash
  , row "previous"        $ previousTxLink wfi.endpointUrl tx
  ]
  where
    -- TODO hack, eliminate; here because the 'previous' field is in the decoded tx body instead of the tx wrapper
    previousTxLink :: ∀ r s m. MonadAff m => URL -> { previous :: HashStr | r } -> ComponentHTML Action s m
    previousTxLink url tx =
      a [ href $ txUrl wfi.endpointUrl tx.previous ] [ text $ txUrl wfi.endpointUrl tx.previous ]

--------------------------------------------------------------------------------

wiringTxBodyRows :: ∀ s m. MonadAff m => WiringFiringInfo -> WiringTx -> Array (ComponentHTML Action s m)
wiringTxBodyRows wfi tx =
  [ row "diagrams"  $ text $ show (tx.wiring.diagrams <#> _.name)
  , row "nets"      $ text $ show (tx.wiring.nets <#> _.name)
  , row "dump"      $ pre [] [ text $ show tx ]
  ]

firingTxBodyRows :: ∀ s m. MonadAff m => WiringFiringInfo -> FiringTx -> Array (ComponentHTML Action s m)
firingTxBodyRows wfi tx =
  [ row "path"      $ text $ show tx.firing.path
  , row "message"   $ text $ maybe "<no message>" show tx.firing.message
  , row "execution" $ text $ maybe "yes" show tx.firing.execution
  ]

--------------------------------------------------------------------------------

row :: ∀ s m. String -> ComponentHTML Action s m -> ComponentHTML Action s m
row caption content =
  tr [] [ td [] [ text caption ]
        , td [] [ content ]
        ]
