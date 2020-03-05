module View.Transaction where

import Prelude hiding (div)
import Affjax (URL) -- TODO eliminate
import Data.Array (mapMaybe)
import Data.String.CodePoints (take)
import Data.Lens (over, preview, _Just, second, view)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (un)
import Data.Either (either)
import Data.Either.Nested (type (\/))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.HTML (HTML, slot, div, table, tr, th, td, a, text, h2, br, pre)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties (classes, href)

import View.Studio.Model (Action(..))
import View.Studio.Model.Route (WiringFiringInfo, ExecutionTrace)
import Statebox.Client (txUrl)
import Statebox.Core.Lenses (_firingTx, _firing, _firingPath, _GluedTransitionId)
import Statebox.Core.Transaction (HashStr, TxSum, FiringTx, TxId, WiringTx, evalTxSum, isExecution)
import Statebox.Core.Types (TID, GluedTransitionId(..))


firingTxView :: ∀ s m. MonadAff m => WiringFiringInfo -> FiringTx -> String \/ ExecutionTrace -> ComponentHTML Action s m
firingTxView wfi tx executionTraceE =
  div [ classes [ ClassName "is-overflowing" ] ]
      [ h2    [] [ text $ if isExecution tx.firing then "Execution" else "Firing" ]
      , table [] $ txWrapperRows wfi tx <>
                   firingTxBodyRows wfi tx <>
                   [ row "trace"     $ text $ either (const "no") (show <<< map (un GluedTransitionId)) firedTransitionsE ] <>
                   [ row "history"   $ either (const $ text "no execution trace") (firingTxHistoryTable wfi.hash) executionTraceE ]
      ]
  where
    firedTransitionsE :: String \/ Array GluedTransitionId
    firedTransitionsE = map (mapMaybe (preview (second <<< _Just <<< _firingTx <<< _firing <<< _firingPath))) executionTraceE

wiringTxView :: ∀ s m. MonadAff m => WiringFiringInfo -> WiringTx -> ComponentHTML Action s m
wiringTxView wfi tx =
  div [ classes [ ClassName "is-overflowing" ] ]
      [ h2    [] [ text $ "Wiring" ]
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

firingTxHistoryTable :: ∀ s m. MonadAff m => TxId -> ExecutionTrace -> ComponentHTML Action s m
firingTxHistoryTable currentHash et =
  div [] (headerRows <> (historyRow <$> et))
  where
    headerRows = [ tr [] [ th [] [ text "hash" ], th [] [ text "transition" ], th [] [ text "message" ] ] ]

    historyRow (hash /\ txMaybe) =
      tr [ classes $ if hash == currentHash then [ ClassName "css-tx-current" ] else [] ] $
         maybe [ td [] [ text $ shortHash hash ], td [] [], td [] [ text $ "transaction not loaded." ] ]
               (evalTxSum (\x ->        [ td [] [ text $ shortHash hash ], td [] [], td [] [ text $ "unexpected non-firing transaction" ] ])
                          (\x ->        [ td [] [ text $ shortHash hash ], td [] [], td [] [ text $ "unexpected non-firing transaction" ] ])
                          (\x ->        [ td [] [ text $ shortHash hash ], td [] [], td [] [ text $ "unexpected non-firing transaction" ] ])
                          (\firingTx -> [ td [] [ text $ shortHash hash ]
                                        , td [] [ text $ show $ view (_GluedTransitionId >>> _firingPath >>> _firing) firingTx ]
                                        , td [] [ maybe (text "no message")
                                                        (\msg -> pre [] [ text $ "\"" <> msg <> "\"" ])
                                                        firingTx.firing.message
                                                ]
                                        ])
                         )
                         txMaybe

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

-- TODO dedupe
shortHash :: HashStr -> String
shortHash = take 8
