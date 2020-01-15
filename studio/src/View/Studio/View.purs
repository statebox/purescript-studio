module View.Studio.View where

import Prelude hiding (div)
import Affjax (URL) -- TODO introduce URL alias in Client so we can abstract Affjax away
import Control.Comonad.Cofree ((:<))
import Data.AdjacencySpace as AdjacencySpace
import Data.AdjacencySpace (AdjacencySpace)
import Data.Array (cons)
import Data.Foldable (foldMap, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String.CodePoints (take)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen (ComponentHTML)
import Halogen.HTML (a, br, div, img, input, li, nav, ol, slot, span, text)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (classes, src, href, placeholder, value)

import Language.Statebox.Wiring.Generator.DiagramV2 (fromOperators) as DiagramV2
import TreeMenu as TreeMenu
import TreeMenu (mkItem, MenuTree, Item)
import Statebox.Core.Transaction (HashStr, TxSum, evalTxSum, isExecutionTx)
import View.Auth.RolesEditor as RolesEditor
import View.Diagram.DiagramEditor as DiagramEditor
import View.Diagram.Model (DiagramInfo)
import View.Diagram.Update as DiagramEditor
import View.KDMonCat.App as KDMonCat.Bricks
import View.KDMonCat.Bricks as KDMonCat.Bricks
import View.Model (Project, NetInfoWithTypesAndRoles)
import View.Petrinet.PetrinetEditor as PetrinetEditor
import View.Petrinet.Model as PetrinetEditor
import View.Studio.Model (Action(..), State, resolveRoute)
import View.Studio.Model.Route (Route, RouteF(..), ResolvedRouteF(..), NodeIdent(..))
import View.Transaction (firingTxView, wiringTxView)
import View.Typedefs.TypedefsEditor as TypedefsEditor

--------------------------------------------------------------------------------

type ChildSlots =
  ( objectTree     :: H.Slot VoidF (TreeMenu.Msg Route) Unit
  , petrinetEditor :: H.Slot VoidF PetrinetEditor.Msg Unit
  , diagramEditor  :: H.Slot VoidF DiagramEditor.Msg Unit
  , kdmoncatBricks :: KDMonCat.Bricks.Slot Unit
  )

_objectTree     = SProxy :: SProxy "objectTree"
_petrinetEditor = SProxy :: SProxy "petrinetEditor"
_diagramEditor  = SProxy :: SProxy "diagramEditor"
_kdmoncatBricks = SProxy :: SProxy "kdmoncatBricks"

data VoidF a

--------------------------------------------------------------------------------

render :: ∀ m. MonadAff m => State -> ComponentHTML Action ChildSlots m
render state =
  div []
    [ navBar
    , div [ classes [ ClassName "flex" ] ]
          [ div [ classes [ ClassName "w-1/6", ClassName "h-12" ] ]
                [ slot _objectTree unit (TreeMenu.menuComponent (_ == state.route)) (stateMenu state) ((\(TreeMenu.Clicked menuNodeId route) -> ShowDiagramNodeContent route) >>> Just) ]
          , div [ classes [ ClassName "w-5/6", ClassName "h-12" ] ]
                [ routeBreadcrumbs state.route
                , resolveRoute state.route state # maybe (text "Couldn't find project/net/diagram.")
                                                         (contentView state.apiUrl)
                ]
          ]
    ]

contentView :: ∀ m. MonadAff m => URL -> ResolvedRouteF Project DiagramInfo NetInfoWithTypesAndRoles -> ComponentHTML Action ChildSlots m
contentView apiUrl route = case route of
  ResolvedHome ->
    homeForm apiUrl

  ResolvedTypes project ->
    TypedefsEditor.typedefsTreeView project.types

  ResolvedAuths project ->
    RolesEditor.roleInfosHtml project.roleInfos

  ResolvedNet netInfo ->
    slot _petrinetEditor unit (PetrinetEditor.ui (Just "main_net")) netInfo (Just <<< HandlePetrinetEditorMsg)

  ResolvedDiagram diagramInfo nodeMaybe ->
    div [ classes [ ClassName "flex" ] ]
        [ div [ classes [ ClassName "w-1/2" ] ]
              [ slot _diagramEditor unit DiagramEditor.ui diagramInfo.ops (Just <<< HandleDiagramEditorMsg) ]
        , div [ classes [ ClassName "w-1/2", ClassName "pl-4" ] ]
              [ slot _kdmoncatBricks unit KDMonCat.Bricks.bricksView bricksInput (Just <<< HandleKDMoncatMsg diagramInfo)
              , case nodeMaybe of
                  Just (NetNode netInfo)          -> slot _petrinetEditor unit (PetrinetEditor.ui (Just "diagram_node")) netInfo (Just <<< HandlePetrinetEditorMsg)
                  Just (DiagramNode diagramInfo2) -> text "TODO viewing internal diagrams is not supported yet."
                  Nothing                         -> text "Click a node to show the corresponding net or diagram."
              ]
        ]
    where
      bricksInput = KDMonCat.Bricks.toBricksInput (DiagramV2.fromOperators diagramInfo.ops) zero

  ResolvedUberRoot url ->
    text $ "Service über-root " <> url

  ResolvedNamespace hash ->
    text $ "Namespace " <> hash

  ResolvedWiring wfi wiringTx ->
    wiringTxView wfi wiringTx

  ResolvedFiring wfi firingTx executionTraceE ->
    firingTxView wfi firingTx executionTraceE

routeBreadcrumbs :: ∀ m. Route -> ComponentHTML Action ChildSlots m
routeBreadcrumbs route =
  nav [ classes $ ClassName <$> [ "css-route-breadcrumbs", "rounded", "font-sans", "w-full", "mt-4", "mb-4" ] ]
      [ ol [ classes $ ClassName <$> [ "list-reset", "flex", "text-grey-dark" ] ] $
           crumb <$> case route of
                       Home                          -> [ "Home" ]
                       Types      projectName        -> [ projectName, "Types" ]
                       Auths      projectName        -> [ projectName, "Authorisation" ]
                       Net        projectName name   -> [ projectName, name ]
                       Diagram    projectName name _ -> [ projectName, name ]
                       UberRootR  url                -> [ "über-namespace", url ]
                       NamespaceR hash               -> [ "namespace", shortHash hash ]
                       WiringR    x                  -> [ x.endpointUrl, "wiring " <> shortHash x.hash ]
                       FiringR    x                  -> [ x.endpointUrl, shortHash x.hash ]
                       DiagramR   hash ix name       -> [ shortHash hash, "diagram " <> show ix <> " " <> name ]
                       NetR       hash ix name       -> [ shortHash hash, "net "     <> show ix <> " " <> name ]
      ]
  where
    crumb str = li [] [ a [ href "#" ] [ text str ] ]

navBar :: ∀ m. ComponentHTML Action ChildSlots m
navBar =
  nav [ classes $ ClassName <$> [ "css-navbar", "flex", "items-center", "justify-between", "flex-wrap", "bg-purple-darker", "p-6" ] ]
  [ div [ classes $ ClassName <$> [ "flex", "items-center", "flex-no-shrink", "text-white", "mr-6" ] ]
            [ img [ src "logo-statebox-white.svg"
                  , classes [ ClassName "css-logo-statebox" ]
                  ]
            , span [ classes $ ClassName <$> [ "navbar-item", "ml-4", "font-semibold", "text-xl" ] ]
                   [ text "Statebox Studio" ]
            ]
      , menu [ "Home"    /\ Just Home
             , "Project" /\ Nothing
             , "Help"    /\ Nothing
             ]
      ]
  where
    menu items =
      div [ classes $ ClassName <$> ["w-full", "block", "flex-grow", "lg:flex", "lg:items-center", "lg:w-auto" ] ]
          [ div [ classes $ ClassName <$> ["text-sm", "lg:flex-grow" ] ]
                (menuItem <$> items)
          ]

    menuItem (label /\ routeMaybe) =
      a (
          [ classes $ ClassName <$> [ "block", "mt-4", "lg:inline-block", "lg:mt-0", "text-purple-lighter", "hover:text-white", "mr-4" ]
          , href "#"
          ]
          <> ((\r -> [ onClick \_ -> Just (SelectRoute r) ]) `foldMap` routeMaybe)
        )
        [ text label ]

--------------------------------------------------------------------------------

stateMenu :: State -> MenuTree Route
stateMenu { projects, apiUrl, hashSpace } =
  mkItem "Studio" Nothing :< (txItems <> projectItems)
  where
    txItems        = AdjacencySpace.unsafeToTree (transactionMenu apiUrl) hashSpace <$> Set.toUnfoldable (AdjacencySpace.rootKeys hashSpace)
    projectItems   = projectMenu <$> projects

projectMenu :: Project -> MenuTree Route
projectMenu p =
  mkItem p.name Nothing :<
    [ mkItem "Types"          (Just $ Types p.name) :< []
    , mkItem "Authorisations" (Just $ Auths p.name) :< []
    , mkItem "Nets"           (Nothing)             :< fromNets     p p.nets
    , mkItem "Diagrams"       (Nothing)             :< fromDiagrams p p.diagrams
    ]
  where
    fromNets     p nets  = (\n -> mkItem n.name (Just $ Net     p.name n.name        ) :< []) <$> nets
    fromDiagrams p diags = (\d -> mkItem d.name (Just $ Diagram p.name d.name Nothing) :< []) <$> diags

-- It's not terribly efficient to construct a Cofree (sub)tree first only to subsequently flatten it, as we do with firings.
transactionMenu :: URL -> AdjacencySpace HashStr TxSum -> HashStr -> Maybe TxSum -> Array (MenuTree Route) -> MenuTree Route
transactionMenu apiUrl t hash valueMaybe itemKids =
  maybe (mkUnloadedItem itemKids)
        (\tx -> mkItem2 hash tx itemKids)
        valueMaybe
  where
    mkItem2 :: HashStr -> TxSum -> Array (MenuTree Route) -> MenuTree Route
    mkItem2 hash tx itemKids = evalTxSum
      (\x -> mkItem ("☁️ "  <> shortHash hash)
                    (Just $ UberRootR apiUrl)
                    :< itemKids
      )
      (\x -> mkItem ("🌐 "  <> shortHash hash)
                    (Just $ NamespaceR x.root.message)
                    :< itemKids
      )
      (\w -> mkItem ("🥨 " <> shortHash hash)
                    (Just $ WiringR { name: hash, endpointUrl: apiUrl, hash: hash })
                    :< (fromNets w.wiring.nets <> fromDiagrams w.wiring.diagrams <> itemKids)
      )
      (\f -> mkItem ((if isExecutionTx f then "🔫 " else "🔥 ") <> shortHash hash)
                    (Just $ FiringR { name: hash, endpointUrl: apiUrl, hash: hash })
                    :< (flattenTree =<< itemKids) -- for nested firings, just drop the 'flattenTree' part
      )
      tx
      where
        fromNets     nets  = mapWithIndex (\ix n -> mkItem ("🔗 " <> n.name) (Just $ NetR     hash ix n.name) :< []) nets
        fromDiagrams diags = mapWithIndex (\ix d -> mkItem ("⛓ " <> d.name) (Just $ DiagramR hash ix d.name) :< []) diags

        flattenTree :: MenuTree Route -> Array (MenuTree Route)
        flattenTree = treeifyElems <<< flattenTree'
          where
            treeifyElems :: Array (Item Route) -> Array (MenuTree Route)
            treeifyElems = map pure

            flattenTree' :: MenuTree Route -> Array (Item Route)
            flattenTree' = foldr cons []

    mkUnloadedItem :: Array (MenuTree Route) -> MenuTree Route
    mkUnloadedItem itemKids = mkItem ("👻 " <> shortHash hash) unloadedRoute :< itemKids
      where
        -- TODO we need to return a Route currently, but we may want to return a (LoadTransaction ... ::Query) instead,
        -- so we could load unloaded hashes from the menu.
        unloadedRoute = Nothing

--------------------------------------------------------------------------------

homeForm :: ∀ m. URL -> ComponentHTML Action ChildSlots m
homeForm apiUrl =
  div []
      [ text "Please select an object from the menu, or enter a transaction hash below."
      , br []
      , br []
      , input [ value ""
              , placeholder "Enter Statebox Cloud transaction hash"
              , onValueInput $ Just <<< LoadTransactions apiUrl
              , classes $ ClassName <$> [ "appearance-none", "w-1/2", "bg-grey-lightest", "text-grey-darker", "border", "border-grey-lighter", "rounded", "py-2", "px-3" ]
              ]
      , br []
      , br []
      , input [ value apiUrl
              , placeholder "Statebox API URL"
              , onValueInput $ Just <<< SetApiUrl
              , classes $ ClassName <$> [ "appearance-none", "w-1/2", "bg-grey-lightest", "text-grey-darker", "border", "border-grey-lighter", "rounded", "py-2", "px-3" ]
              ]
      , br []
      , br []
      , text "Alternatively, you can load a PNPRO file."
      , br []
      , br []
      , input [ value ""
              , placeholder "Enter http URL to PNPRO file"
              , onValueInput $ Just <<< LoadPNPRO
              , classes $ ClassName <$> [ "appearance-none", "w-1/2", "bg-grey-lightest", "text-grey-darker", "border", "border-grey-lighter", "rounded", "py-2", "px-3" ]
              ]
      ]

--------------------------------------------------------------------------------

shortHash :: HashStr -> String
shortHash = take 8
