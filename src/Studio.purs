module Studio where

import Prelude hiding (div)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen as H
import Halogen (ParentDSL, ParentHTML)
import Halogen.Component.ChildPath as ChildPath
import Halogen.HTML as HH
import Halogen.HTML (HTML, nav, div, h1, p, a, img, text)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, src)
import Effect.Aff.Class (class MonadAff)

import Model (Project, PID, TID, NetInfo, emptyNetInfo, QueryF(..), Msg(NetUpdated))
import DiagramEditor.DiagramEditor as DiagramEditor
import DiagramEditor.Update as DiagramEditor
import PetrinetView as PetrinetView
import ExampleData as Ex

type State =
  { project1   :: Project
  , msg        :: String
  , activeView :: ActiveView
  }

data Query a
  = ShowView ActiveView a
  | HandlePetrinetEditorMsg Msg a
  | SendPetrinetEditorMsg (QueryF PID TID a) a
  | HandleDiagramEditorMsg Unit a

-- TODO this is probably doable with one of the slot-like things, or Childpath or sth
data ActiveView = PetrinetEditor | DiagramEditor

--------------------------------------------------------------------------------

type ChildQuery = Coproduct2 (QueryF PID TID) DiagramEditor.Query

type ChildSlot = Either2 Unit Unit

petrinetViewSlotPath = ChildPath.cp1
diagramEditorSlotPath = ChildPath.cp2

--------------------------------------------------------------------------------

ui :: forall m. MonadAff m => H.Component HTML Query Unit Void m
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
      { msg:        "Welcome to Statebox Studio!"
      , project1:   Ex.project1
      , activeView: PetrinetEditor
      }

    eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void m
    eval = case _ of
      ShowView v next -> do
        H.modify_ (\state -> state { activeView = v })
        pure next

      HandlePetrinetEditorMsg NetUpdated next -> do
        -- TODO
        pure next

      SendPetrinetEditorMsg kidQuery next -> case kidQuery of
        LoadNet net _ -> do
          x <- H.query' petrinetViewSlotPath unit $ H.action (LoadNet net)
          pure next
        _ -> do
          pure next -- TODO do we need to handle other cases?

      HandleDiagramEditorMsg unit next -> do
        pure next

    render :: State -> ParentHTML Query ChildQuery ChildSlot m
    render state =
      div []
        [ navBar
        , div [ classes [ ClassName "columns" ] ]
              [ div [ classes [ ClassName "column", ClassName "is-2" ] ]
                    [ netChooser state.project1.nets ]
              , div [ classes [ ClassName "column" ] ]
                    [ case state.activeView of
                        PetrinetEditor ->
                          HH.slot' petrinetViewSlotPath unit (PetrinetView.ui state.project1.allRoleInfos emptyNetInfo) unit (HE.input HandlePetrinetEditorMsg)
                        DiagramEditor  ->
                          HH.slot' diagramEditorSlotPath unit DiagramEditor.ui unit (HE.input HandleDiagramEditorMsg)
                    ]
              ]
        ]

      where
        netChooser :: Array NetInfo -> ParentHTML Query ChildQuery ChildSlot m
        netChooser items =
          nav [ classes [ ClassName "panel" ] ] $
              [ p [ classes [ ClassName "panel-heading" ] ] [ text state.project1.name ] ]
              <> (instanceListItem isSelected <$> items)
          where
            isSelected :: NetInfo -> Boolean
            isSelected = const false -- TODO

        instanceListItem :: (NetInfo -> Boolean) -> NetInfo -> ParentHTML Query ChildQuery ChildSlot m
        instanceListItem isSelected netInfo =
          a [ classes [ ClassName "panel-block"
                      , ClassName $ guard (isSelected netInfo) "is-active"
                      ]
            , onClick (HE.input_ (SendPetrinetEditorMsg (LoadNet netInfo.net unit)))
            ]
            [ text netInfo.name ]

        navBar :: ParentHTML Query ChildQuery ChildSlot m
        navBar =
          nav [ classes [ ClassName "navbar" ] ]
              [ div [ classes [ ClassName "navbar-brand" ] ]
                    [ a [ classes [ ClassName "navbar-item" ] ]
                        [ img [ src "logo-statebox.jpg" ]
                        ]
                    ]
              , div [ classes [ ClassName "navbar-menu" ] ]
                    [ div [ classes [ ClassName "navbar-start" ] ]
                          [ div [ classes [ ClassName "navbar-item" ] ]
                                [ h1 [] [ text "Statebox Studio" ] ]
                          , a [ classes [ ClassName "navbar-item" ]
                              , onClick (HE.input_ (ShowView PetrinetEditor))
                              ]
                              [ text "Petri net editor" ]
                          , a [ classes [ ClassName "navbar-item" ]
                              , onClick (HE.input_ (ShowView DiagramEditor))
                              ]
                              [ text "String diagram editor" ]
                          ]
                    , div [ classes [ ClassName "navbar-end" ] ]
                          [ a   [ classes [ ClassName "navbar-item" ] ] [ text "Development" ] ]
                    ]
              ]
