module View.Studio.View where

import Prelude hiding (div)
import Affjax (URL) -- TODO introduce URL alias in Client so we can abstract Affjax away
import Control.Comonad.Cofree ((:<))
import Data.AdjacencySpace as AdjacencySpace
import Data.AdjacencySpace (AdjacencySpace)
import Data.Array (concat, cons, last)
import Data.Foldable (class Foldable, foldMap, foldr, length, null)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Set as Set
import Data.String.CodePoints (take)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen (ComponentHTML)
import Halogen.HTML (a, button, div, fieldset, h1, h3, input, legend, li, nav, ol, p_, slot, span, text, ul, datalist, option)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (classes, id_, href, placeholder, value, tabIndex, list, type_, InputType(InputText))
import Web.UIEvent.MouseEvent (toEvent)

import Language.Statebox.Wiring.Generator.DiagramV2.Operators (fromOperators, toPixel) as DiagramV2
import TreeMenu as TreeMenu
import TreeMenu (mkItem, mkEditItem, MenuTree, Item, mapMenuTreeRoutes, WrapAction(..))
import Statebox.Core.Transaction (HashStr, TxSum, evalTxSum, isExecutionTx)
import Statebox.Core.Types (NetsAndDiagramsIndex(..))
import View.Auth.RolesEditor as RolesEditor
import View.CRUDAction
import View.Diagram.DiagramEditor as DiagramEditor
import View.Diagram.Update as DiagramEditor
import View.KDMonCat.App as KDMonCat.App
import View.KDMonCat.Bricks as KDMonCat.Bricks
import View.Model (Project, ProjectId, emptyProject)
import View.Petrinet.PetrinetEditor as PetrinetEditor
import View.Petrinet.Model as PetrinetEditor
import View.Studio.Model
import View.Studio.Model.Route
import View.Transaction (firingTxView, wiringTxView)
import View.Typedefs.TypedefsEditor as TypedefsEditor

--------------------------------------------------------------------------------

type ChildSlots =
  ( objectTree     :: H.Slot VoidF Action Unit
  , petrinetEditor :: H.Slot VoidF PetrinetEditor.Msg Unit
  , diagramEditor  :: H.Slot VoidF DiagramEditor.Msg Unit
  , kdmoncatApp    :: KDMonCat.App.Slot Unit
  , kdmoncatBricks :: KDMonCat.Bricks.Slot Unit
  )

_objectTree     = SProxy :: SProxy "objectTree"
_petrinetEditor = SProxy :: SProxy "petrinetEditor"
_diagramEditor  = SProxy :: SProxy "diagramEditor"
_kdmoncatBricks = SProxy :: SProxy "kdmoncatBricks"
_kdmoncatApp    = SProxy :: SProxy "kdmoncatApp"

data VoidF a

--------------------------------------------------------------------------------

render :: ∀ m. MonadAff m => State -> ComponentHTML Action ChildSlots m
render state =
  div [ classes [ ClassName "studio" ] ]
    [ navBar state.title state.menuItems
    , div [ classes [ ClassName "has-columns" ] ] (sidebar <> main)
    ]
  where
    menu = stateMenu state
    buttons = sidebarButtons state
    showSidebar = length menu > 1 || length buttons > 0
    sidebar = guard showSidebar $
      [ nav [ classes [ ClassName "stbx-sidebar" ] ]
            [ slot _objectTree unit TreeMenu.menuComponent
                { tree: menu
                , isSelected: (_ == state.route)
                , editMode: state.navEditMode
                , onVisit: ShowDiagramNodeContent
                } Just
            , div [] buttons ]
      ]
    main =
      [ div []
            [ div [ classes $ ClassName <$> [ "container", "is-flex", "has-rows" ] <> guard showSidebar [ "is-fluid" ] ] $
                  resolveRoute state.route state # \resolved ->
                    [ routeBreadcrumbs state.route resolved
                    , contentView state.apiUrl resolved
                    ]
            ]
      ]

contentView :: ∀ m. MonadAff m => URL -> ResolvedRoute -> ComponentHTML Action ChildSlots m
contentView apiUrl route = last route # maybe (text "Couldn't find project/net/diagram.") view
  where
    view = case _ of
      ResolvedHome projects ->
        div []
            [ projectsDashboard projects
            ]

      ResolvedTxHome projects ->
        div []
            [ homeForm apiUrl
            ]

      ResolvedProject project ->
        div []
            [ p_ [ text $ "Project '" <> project.name <> "'" ]
            ]

      ResolvedTypes project ->
        TypedefsEditor.typedefsTreeView project.types

      ResolvedAuths project ->
        RolesEditor.roleInfosHtml project.roleInfos

      ResolvedNet netInfo ->
        slot _petrinetEditor unit (PetrinetEditor.ui (Just "main_net")) netInfo (Just <<< HandlePetrinetEditorMsg)

      ResolvedDiagram diagramInfo nodeMaybe ->
        div [ classes [ ClassName "diagram-editor", ClassName "has-columns" ] ]
            [ div []
                  [ slot _diagramEditor unit DiagramEditor.ui diagramInfo.ops (Just <<< HandleDiagramEditorMsg) ]
            , div []
                  [ slot _kdmoncatBricks unit KDMonCat.Bricks.bricksView bricksInput (Just <<< HandleKDMonCatBricksMsg diagramInfo)
                  , case nodeMaybe of
                      Just (NetNode netInfo)          -> slot _petrinetEditor unit (PetrinetEditor.ui (Just "diagram_node")) netInfo (Just <<< HandlePetrinetEditorMsg)
                      Just (DiagramNode diagramInfo2) -> text "TODO viewing internal diagrams is not supported yet."
                      Nothing                         -> text "Click a node to show the corresponding net or diagram."
                  ]
            ]
        where
          bricksInput =
            (KDMonCat.App.toBricksInput (DiagramV2.fromOperators diagramInfo.ops) zero)
            { renderBoxContent = \name bid ->
                (KDMonCat.Bricks.defaultRenderBoxContent name bid)
                { className = if maybeSelectedBid == Just bid then "selected" else "" } }
          maybeSelectedBid = case nodeMaybe of
            Just (NetNode netInfo) -> DiagramV2.toPixel diagramInfo.ops (\{ identifier } -> netInfo.name == identifier)
            _ -> Nothing

      ResolvedKDMonCat kid { input } ->
        slot _kdmoncatApp unit KDMonCat.App.appView input (Just <<< HandleKDMonCatAppMsg kid)

      ResolvedUberRoot url ->
        text $ "Service über-root " <> url

      ResolvedNamespace hash ->
        text $ "Namespace " <> hash

      ResolvedWiring wfi wiringTx ->
        wiringTxView wfi wiringTx

      ResolvedFiring wfi firingTx executionTraceE ->
        firingTxView wfi firingTx executionTraceE

routeBreadcrumbs :: ∀ m. Route -> ResolvedRoute -> ComponentHTML Action ChildSlots m
routeBreadcrumbs route resolvedRoute =
  nav [ classes [ ClassName "stbx-breadcrumbs" ] ]
      [ ol [] $ toBreadcrumb <$> resolvedRoute ]
  where
    crumb r str = li [] [ a [ onClick \_ -> Just (SelectRoute r) ] [ text str ] ]

    sub = case route of
      Home -> Home
      TxHome h -> TxHome h
      ProjectRoute id _ -> ProjectRoute id ProjectHome
      ApiRoute _ url -> ApiRoute UberRootR url

    toBreadcrumb = case _ of
      ResolvedHome      _ -> crumb Home "Home"
      ResolvedTxHome    _ -> crumb (TxHome Nothing) "Home"

      ResolvedProject   p -> crumb sub p.name
      ResolvedTypes     p -> crumb route "Types"
      ResolvedAuths     p -> crumb route "Authorisation"

      ResolvedNet       n -> crumb route n.name
      ResolvedDiagram   d _ -> crumb route d.name
      ResolvedKDMonCat  _ k -> crumb route k.name

      ResolvedUberRoot  u -> crumb sub u
      ResolvedNamespace h -> crumb route $ shortHash h
      ResolvedWiring    w _ -> crumb route $ "wiring " <> shortHash w.hash
      ResolvedFiring    f _ _ -> crumb route $ "firing " <> shortHash f.hash

navBar :: ∀ m. String -> Array (String /\ Maybe Route) -> ComponentHTML Action ChildSlots m
navBar title menuItems =
  nav [ classes [ ClassName "stbx-menu" ], tabIndex 0 ]
      [ span [ classes [ ClassName "stbx-menu-close" ], tabIndex 0 ] []
      , ul [] $
           [ li [] [ h1 [] [ text title ] ] ]
           <> (menuItem <$> menuItems)
      , a [ href "https://statebox.org", classes [ ClassName "stbx-logo" ] ] []
      ]
  where
    menuItem (label /\ routeMaybe) =
      li []
         [ a (
               [ href "#" ]
               <> ((\r -> [ onClick \_ -> Just (SelectRoute r) ]) `foldMap` routeMaybe)
             )
             [ text label ]
         ]

--------------------------------------------------------------------------------

stateMenu :: State -> MenuTree Action Route
stateMenu { projects, apiUrl, hashSpace, route } =
  case route of
    ProjectRoute pid _ -> findProject projects pid # maybe (mkItem "Studio" Nothing :< []) (projectMenu >>> mapMenuTreeRoutes (ProjectRoute pid))
    _ -> mkItem "Studio" Nothing :< map (mapMenuTreeRoutes (\x -> ApiRoute x apiUrl)) txItems
  where
    txItems        = AdjacencySpace.unsafeToTree (transactionMenu apiUrl) hashSpace <$> Set.toUnfoldable (AdjacencySpace.rootKeys hashSpace)

projectMenu :: Project -> MenuTree Action ProjectRoute
projectMenu p =
  mkItem p.name (Just ProjectHome) :< concat
    [ when p.types     (mkItem "Types"          (Just Types) :< [])
    , when p.roleInfos $ mkItem "Authorisations" (Just Auths) :< []
    , when p.nets      $ mkItem "Nets"           (Nothing)    :< fromNets      p.nets
    , when p.diagrams  $ mkItem "Wirings"        (Nothing)    :< fromDiagrams  p.diagrams
    , when p.kdmoncats $ mkItem "Diagrams"       (Nothing)    :< fromKDMonCats (p.kdmoncats # Map.toUnfoldable)
    ]
  where
    when :: ∀ f a. Foldable f => f a -> MenuTree Action ProjectRoute -> Array (MenuTree Action ProjectRoute)
    when xs item = if null xs then [] else [item]
    fromNets      nets  = (\n          -> mkItem n.name (Just $ Net       n.name        ) :< []) <$> nets
    fromDiagrams  diags = (\d          -> mkItem d.name (Just $ Diagram   d.name Nothing) :< []) <$> diags
    fromKDMonCats diags = (\(kid /\ k) -> mkEditItem k.name (Just $ KDMonCatR kid) (Just \(WrapAction f) -> CRUDKDMonCat $ f kid) :< []) <$> diags

-- It's not terribly efficient to construct a Cofree (sub)tree first only to subsequently flatten it, as we do with firings.
transactionMenu :: URL -> AdjacencySpace HashStr TxSum -> HashStr -> Maybe TxSum -> Array (MenuTree Action ApiRoute) -> MenuTree Action ApiRoute
transactionMenu endpointUrl t hash valueMaybe itemKids =
  maybe mkUnloadedItem mkItem2 valueMaybe
  where
    mkItem2 :: TxSum -> MenuTree Action ApiRoute
    mkItem2 tx = evalTxSum
      (\x -> mkItem ("☁️ "  <> shortHash hash)
                    (Just UberRootR)
                    :< itemKids
      )
      (\x -> mkItem ("🌐 "  <> shortHash hash)
                    (Just $ NamespaceR x.root.message)
                    :< itemKids
      )
      (\w -> mkItem ("🥨 " <> shortHash hash)
                    (Just $ WiringR hash)
                    :< (fromNets w.wiring.nets <> fromDiagrams w.wiring.diagrams <> itemKids)
      )
      (\f -> mkItem ((if isExecutionTx f then "🔫 " else "🔥 ") <> shortHash hash)
                    (Just $ FiringR hash)
                    :< (flattenTree =<< itemKids) -- for nested firings, just drop the 'flattenTree' part
      )
      tx
      where
        fromNets     nets  = mapWithIndex (\ix n -> mkItem ("🔗 " <> n.name) (Just $ NetR     hash (NetsAndDiagramsIndex ix) n.name) :< []) nets
        fromDiagrams diags = mapWithIndex (\ix d -> mkItem ("⛓ " <> d.name) (Just $ DiagramR hash (NetsAndDiagramsIndex ix) d.name) :< []) diags

        flattenTree :: MenuTree Action ApiRoute -> Array (MenuTree Action ApiRoute)
        flattenTree = treeifyElems <<< flattenTree'
          where
            treeifyElems :: Array (Item Action ApiRoute) -> Array (MenuTree Action ApiRoute)
            treeifyElems = map pure

            flattenTree' :: MenuTree Action ApiRoute -> Array (Item Action ApiRoute)
            flattenTree' = foldr cons []

    mkUnloadedItem :: MenuTree Action ApiRoute
    mkUnloadedItem = mkItem ("👻 " <> shortHash hash) unloadedRoute :< itemKids
      where
        -- TODO we need to return an ApiRoute currently, but we may want to return a (LoadTransaction ... :: Query) instead,
        -- so we could load unloaded hashes from the menu.
        unloadedRoute = Nothing

sidebarButtons :: ∀ m. State -> Array (ComponentHTML Action ChildSlots m)
sidebarButtons state = case state.route of
  (ProjectRoute _ _) ->
    [ button [ onClick \_ -> Just $ CRUDKDMonCat $ CreateAction { name: "Untitled", input: mempty } ]
             [ text "＋" ]
    , button [ onClick $ const $ Just $ ToggleEditMode ]
             [ text $ if state.navEditMode then "Done" else "Edit" ]
    ]
  _ -> []

--------------------------------------------------------------------------------

projectsDashboard :: ∀ m. Map.Map ProjectId Project -> ComponentHTML Action ChildSlots m
projectsDashboard projects =
  div []
      [ ul [ classes [ ClassName "stbx-cards" ] ] $
          (projects # foldMapWithIndex mkProjectLink) <>
          [ li [ classes [ ClassName "stbx-add-card" ] ]
               [ button [ onClick $ Just <<< StopEvent (Just $ CRUDProject $ CreateAction $ emptyProject { name = "New Project" }) <<< toEvent ]
                        [ text "Create new project" ]
               ]
          ]
        ]
  where
    mkProjectLink projectId p = pure $
      li [ onClick \_ -> Just (SelectRoute (ProjectRoute projectId ProjectHome))]
         [ h3 [] [ input [ value p.name
                         , type_ InputText
                         , onValueInput \v -> Just $ CRUDProject $ UpdateAction _{ name = v } projectId
                         , onClick $ Just <<< StopEvent Nothing <<< toEvent
                         ]
                 ]
         , div [ classes [ ClassName "hover-controls" ] ]
               [ button [ onClick $ Just <<< StopEvent (Just $ CRUDProject $ DeleteAction projectId) <<< toEvent ]
                        [ text "Delete" ]
               ]
         ]

homeForm :: ∀ m. URL -> ComponentHTML Action ChildSlots m
homeForm apiUrl =
  div []
      [ fieldset []
                 [ legend [] [ text "Please select an object from the menu, or enter a transaction hash below." ]
                 , p_ [ input [ value ""
                              , type_ InputText
                              , placeholder "Enter Statebox Cloud transaction hash"
                              , onValueInput $ Just <<< LoadTransactions apiUrl
                              ]
                      ]
                 , p_ [ input [ value apiUrl
                              , type_ InputText
                              , placeholder "Statebox API URL"
                              , onValueInput $ Just <<< SetApiUrl
                              , list "endpoint-urls"
                              ]
                      , datalist [ id_ "endpoint-urls" ]
                                 [ option [ value "https://testapi.statebox.io" ] [] ]
                      ]
                 ]
              --  , fieldset []
              --             [ legend [] [ text "Alternatively, you can load a PNPRO file." ]
              --             , p []
              --                 [ input [ value ""
              --                         , type_ InputText
              --                         , placeholder "Enter http URL to PNPRO file"
              --                         , onValueInput $ Just <<< LoadPNPRO
              --                         ]
              --                 ]
              --             ]
      ]

--------------------------------------------------------------------------------

shortHash :: HashStr -> String
shortHash = take 8
