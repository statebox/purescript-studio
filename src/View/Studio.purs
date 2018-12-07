module Studio where

import Prelude hiding (div)
import Data.Array (catMaybes)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen as H
import Halogen (ParentDSL, ParentHTML)
import Halogen.Component.ChildPath as ChildPath
import Halogen.HTML as HH
import Halogen.HTML (HTML, nav, div, h1, p, a, img, text, ul, li)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, src, href)
import Halogen.HTML.Properties.ARIA as ARIA
import Effect.Aff.Class (class MonadAff)

import View.Petrinet.Model (Project, PID, TID, NetInfo, emptyNetInfo, NetObj, QueryF(..), Msg(NetUpdated))
import View.Diagram.DiagramEditor as DiagramEditor
import View.Diagram.Model (DiagramInfo)
import View.Diagram.Update as DiagramEditor
import View.Petrinet.PetrinetEditor as PetrinetEditor
import View.Petrinet.Model as PetrinetEditor
import ExampleData as Ex

type State =
  { project1   :: Project
  , focusedNet :: Maybe NetInfo
  , activeView :: ActiveView
  , msg        :: String
  }

data Query a
  = SelectNet NetInfo a
  | SelectDiagram a
  | HandlePetrinetEditorMsg Msg a
  | HandleDiagramEditorMsg Unit a

-- TODO this is probably doable with one of the slot-like things, or Childpath or sth
data ActiveView = PetrinetEditor | DiagramEditor

--------------------------------------------------------------------------------

type ChildQuery = Coproduct2 (PetrinetEditor.QueryF PID TID) DiagramEditor.Query

type ChildSlot = Either2 Unit Unit

petrinetEditorSlotPath = ChildPath.cp1
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
      , focusedNet: Nothing
      , activeView: PetrinetEditor
      }

    eval :: Query ~> ParentDSL State Query ChildQuery ChildSlot Void m
    eval = case _ of
      HandlePetrinetEditorMsg NetUpdated next -> do
        -- TODO
        pure next

      SelectNet netInfo next -> do
        H.modify_ (\state -> state { focusedNet = pure netInfo, activeView = PetrinetEditor })
        x <- H.query' petrinetEditorSlotPath unit $ H.action (LoadNet netInfo)
        pure next

      SelectDiagram next -> do
        H.modify_ (\state -> state { activeView = DiagramEditor })
        pure next

      HandleDiagramEditorMsg unit next -> do
        pure next

    render :: State -> ParentHTML Query ChildQuery ChildSlot m
    render state =
      div []
        [ navBar
        , div [ classes [ ClassName "columns" ] ]
              [ div [ classes [ ClassName "column", ClassName "is-2" ] ]
                    [ objectChooser (\ni -> Just ni.name == (state.focusedNet <#> _.name)) state.project1.nets ]
              , div [ classes [ ClassName "column" ] ]
                    [ pathBreadcrumbs
                    , case state.activeView of
                        PetrinetEditor ->
                          HH.slot' petrinetEditorSlotPath unit (PetrinetEditor.ui state.project1.allRoleInfos emptyNetInfo) unit (HE.input HandlePetrinetEditorMsg)
                        DiagramEditor  ->
                          HH.slot' diagramEditorSlotPath unit DiagramEditor.ui unit (HE.input HandleDiagramEditorMsg)
                    ]
              ]
        ]

      where
        pathBreadcrumbs :: ParentHTML Query ChildQuery ChildSlot m
        pathBreadcrumbs =
          nav [ classes [ ClassName "breadcrumb has-arrow-separator", ClassName "is-small" ]
              , ARIA.label "breadcrumbs"
              ]
              [ ul [] (crumb <$> catMaybes  [ Just state.project1.name
                                            , state.focusedNet <#> _.name
                                            ]) ]
          where
            crumb str = li [] [ a [ href "" ] [ text str ] ]

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
                                [ h1 [ classes [ ClassName "subtitle" ] ] [ text "Statebox Studio" ] ]
                          ]
                    , div [ classes [ ClassName "navbar-end" ] ]
                          [ a   [ classes [ ClassName "navbar-item" ] ] [ text "Development" ] ]
                    ]
              ]

        objectChooser :: (NetInfo -> Boolean) -> Array NetInfo -> ParentHTML Query ChildQuery ChildSlot m
        objectChooser isSelected nets =
          nav [ classes [ ClassName "panel" ] ] $
              [ p [ classes [ ClassName "panel-heading" ] ] [ text state.project1.name ] ]
              <> (netItem isSelected <$> nets)
              <> [ p [ classes [ ClassName "panel-heading" ] ] [ text "Diagrams" ] ]
              <> (diagramItem (const false) <$> Ex.diagrams)
          where
            netItem :: (NetInfo -> Boolean) -> NetInfo -> ParentHTML Query ChildQuery ChildSlot m
            netItem isSelected netInfo =
              a [ classes [ ClassName "panel-block"
                          , ClassName $ guard (isSelected netInfo) "is-active"
                          ]
                , onClick (HE.input_ (SelectNet netInfo))
                ]
                [ text netInfo.name ]

            diagramItem :: (DiagramInfo -> Boolean) -> DiagramInfo -> ParentHTML Query ChildQuery ChildSlot m
            diagramItem isSelected d =
              a [ classes [ ClassName "panel-block"
                          , ClassName $ guard (isSelected d) "is-active"
                          ]
                , onClick (HE.input_ (SelectDiagram))
                ]
                [ text d.name ]
