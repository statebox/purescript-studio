module Studio where

import Prelude hiding (div)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen as H
import Halogen (ParentDSL, ParentHTML)
import Halogen.HTML as HH
import Halogen.HTML (HTML, nav, div, h1, p, a, img, text)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, src)
import Effect.Aff.Class (class MonadAff)

import Model (PID, TID, NetInfo, emptyNetInfo, QueryF(..), Msg(NetUpdated))
import PetrinetView as PetrinetView
import ExampleData as Ex

type State =
  { nets :: Array NetInfo
  , msg  :: String
  }

data Query a
  = HandleViewerMsg Msg a
  | SendViewerMsg (QueryF PID TID a) a

data Slot = PetrinetViewSlot
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

ui :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState =
      { msg:  "Welcome to Statebox Studio!"
      , nets: [ { name: "Traffic lights"    , net: Ex.net1, netApi: Ex.netApi1 }
              , { name: "Producer-consumer" , net: Ex.net2, netApi: Ex.netApi2 }
              ]
              <> Ex.pnproNetInfos1
      }

    eval :: Query ~> ParentDSL State Query (QueryF PID TID) Slot Void m
    eval = case _ of
      HandleViewerMsg NetUpdated next -> do
        -- TODO
        pure next

      SendViewerMsg kidQuery next -> case kidQuery of
        LoadNet net _ -> do
          x <- H.query PetrinetViewSlot $ H.action (LoadNet net)
          pure next
        _ -> do
          pure next -- TODO do we need to handle other cases?

    render :: State -> ParentHTML Query (QueryF PID TID) Slot m
    render state =
      div []
        [ navBar
        , div [ classes [ ClassName "columns" ] ]
              [ div [ classes [ ClassName "column", ClassName "is-2" ] ]
                    [ netChooser state.nets ]
              , div [ classes [ ClassName "column" ] ]
                    [ HH.slot PetrinetViewSlot (PetrinetView.ui emptyNetInfo) unit (HE.input HandleViewerMsg) ]
              ]
        ]

      where
        netChooser :: Array NetInfo -> ParentHTML Query (QueryF PID TID) Slot m
        netChooser items =
          nav [ classes [ ClassName "panel" ] ] $
              [ p [ classes [ ClassName "panel-heading" ] ] [ text "Petri nets" ] ]
              <> (instanceListItem isSelected <$> items)
          where
            isSelected :: NetInfo -> Boolean
            isSelected = const false -- TODO

        instanceListItem :: (NetInfo -> Boolean) -> NetInfo -> ParentHTML Query (QueryF PID TID) Slot m
        instanceListItem isSelected netInfo =
          a [ classes [ ClassName "panel-block"
                      , ClassName $ guard (isSelected netInfo) "is-active"
                      ]
            , onClick (HE.input_ (SendViewerMsg (LoadNet netInfo.net unit)))
            ]
            [ text netInfo.name ]

        navBar :: ParentHTML Query (QueryF PID TID) Slot m
        navBar =
          nav [ classes [ ClassName "navbar" ] ]
              [ div [ classes [ ClassName "navbar-brand" ] ]
                    [ a [ classes [ ClassName "navbar-item" ] ]
                        [ img [ src "logo-statebox.jpg" ]
                        ]
                    ]
              , div [ classes [ ClassName "navbar-menu" ] ]
                    [ div [ classes [ ClassName "navbar-start" ] ]
                          [ div [ classes [ ClassName "navbar-item" ] ] [ h1 [] [ text "Statebox Studio" ] ] ]
                    , div [ classes [ ClassName "navbar-end" ] ]
                          [ a   [ classes [ ClassName "navbar-item" ] ] [ text "Development" ] ]
                    ]
              ]
