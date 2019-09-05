module View.Studio.View where

import Prelude hiding (div)
import Control.Comonad.Cofree ((:<))
import Data.Array (cons)
import Data.AdjacencySpace as AdjacencySpace
import Data.AdjacencySpace (AdjacencySpace)
import Data.Either (either)
import Data.Foldable (foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String.CodePoints (take)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception (try)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen (ComponentHTML)
import Halogen.HTML (HTML, slot, input, nav, div, h1, p, a, img, text, ul, ol, li, aside, span, i, br, pre)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (classes, src, href, placeholder)
import Halogen.Query.HalogenM (HalogenM)
import TreeMenu as MenuTree
import TreeMenu (mkItem, MenuTree)

import Data.Petrinet.Representation.PNPRO as PNPRO
import Statebox.Client as Stbx
import Statebox.Client (evalTransactionResponse)
import Statebox.Core.Transaction as Stbx
import Statebox.Core.Transaction (HashStr, HashTx, TxSum, evalTxSum)
import Statebox.Core.Transaction.Codec (DecodingError(..))
import View.Auth.RolesEditor as RolesEditor
import View.Diagram.DiagramEditor as DiagramEditor
import View.Diagram.Model (DiagramInfo)
import View.Diagram.Update as DiagramEditor
import View.Model (Project, NetInfoWithTypesAndRoles)
import View.Petrinet.PetrinetEditor as PetrinetEditor
import View.Petrinet.Model as PetrinetEditor
import View.Petrinet.Model (Msg(NetUpdated))
import View.Studio.Model (Action(..), State, fromPNPROProject, resolveRoute)
import View.Studio.Model.Route (Route, RouteF(..), ResolvedRouteF(..), NodeIdent(..))
import View.Typedefs.TypedefsEditor as TypedefsEditor

import ExampleData as Ex

--------------------------------------------------------------------------------

type ChildSlots =
  ( objectTree     :: H.Slot VoidF (MenuTree.Msg Route) Unit
  , petrinetEditor :: H.Slot VoidF PetrinetEditor.Msg Unit
  , diagramEditor  :: H.Slot VoidF DiagramEditor.Msg Unit
  )

_objectTree     = SProxy :: SProxy "objectTree"
_petrinetEditor = SProxy :: SProxy "petrinetEditor"
_diagramEditor  = SProxy :: SProxy "diagramEditor"

data VoidF a

--------------------------------------------------------------------------------

render :: ∀ m. MonadAff m => State -> ComponentHTML Action ChildSlots m
render state =
  div []
    [ navBar
    , div [ classes [ ClassName "flex" ] ]
          [ div [ classes [ ClassName "w-1/6", ClassName "h-12" ] ]
                [ slot _objectTree unit (MenuTree.menuComponent (_ == state.route)) (stateMenu state) ((\(MenuTree.Clicked menuNodeId route) -> ShowDiagramNodeContent route) >>> Just) ]
          , div [ classes [ ClassName "w-5/6", ClassName "h-12" ] ]
                [ routeBreadcrumbs state.route
                , maybe (text "Couldn't find project/net/diagram.") mainView (resolveRoute state.route state)
                ]
          ]
    ]

mainView :: ∀ m. MonadAff m => ResolvedRouteF Project DiagramInfo NetInfoWithTypesAndRoles -> ComponentHTML Action ChildSlots m
mainView route = case route of
  ResolvedHome ->
    div []
        [ text "Please select an object from the menu, or enter a transaction hash below."
        , br [], br []
        , input [ HP.value ""
                , placeholder "Enter Statebox Cloud transaction hash"
                , HE.onValueInput $ Just <<< LoadTransactions Ex.endpointUrl
                , classes $ ClassName <$> [ "appearance-none", "w-1/2", "bg-grey-lightest", "text-grey-darker", "border", "border-grey-lighter", "rounded", "py-2", "px-3" ]
                ]
        , br []
        , br []
        , input [ HP.value ""
                , placeholder "Enter http URL to PNPRO file"
                , HE.onValueInput $ Just <<< LoadPNPRO
                , classes $ ClassName <$> [ "appearance-none", "w-1/2", "bg-grey-lightest", "text-grey-darker", "border", "border-grey-lighter", "rounded", "py-2", "px-3" ]
                ]
        ]
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
              [ case nodeMaybe of
                  Just (NetNode netInfo)          -> slot _petrinetEditor unit (PetrinetEditor.ui (Just "diagram_node")) netInfo (Just <<< HandlePetrinetEditorMsg)
                  Just (DiagramNode diagramInfo2) -> text "TODO viewing internal diagrams is not supported yet."
                  Nothing                         -> text "Click a node to show the corresponding net or diagram."
              ]
        ]

  ResolvedUberRoot url ->
    text $ "Service über-root " <> url

  ResolvedNamespace hash ->
    text $ "Namespace " <> hash

  ResolvedWiring wfi wiringTx ->
    div []
        [ text $ "Wiring " <> wfi.hash <> " at " <> wfi.endpointUrl <> "."
        , br [], br []
        , pre [] [ text $ show wiringTx ]
        ]

  ResolvedFiring wfi firingTx ->
    div []
        [ text $ "Firing " <> wfi.hash <> " at " <> wfi.endpointUrl <> "."
        , br []
        , p [] [ pre [] [ text $ show firingTx ] ]
        , br []
        , p [] [ text $ "message: " <> show firingTx.firing.message ]
        , br []
        , p [] [ text $ "path: " <> show firingTx.firing.path ]
        ]

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
          <> ((\r -> [ HE.onClick \_ -> Just (SelectRoute r) ]) `foldMap` routeMaybe)
        )
        [ text label ]

stateMenu :: State -> MenuTree Route
stateMenu { projects, hashSpace } =
  mkItem "Studio" Nothing :< (txItems <> projectItems)
  where
    txItems        = AdjacencySpace.unsafeToTree transactionMenu hashSpace <$> Set.toUnfoldable (AdjacencySpace.rootKeys hashSpace)
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

transactionMenu :: AdjacencySpace HashStr TxSum -> HashStr -> Maybe TxSum -> Array (MenuTree Route) -> MenuTree Route
transactionMenu t hash valueMaybe itemKids =
  maybe (mkUnloadedItem itemKids)
        (\tx -> mkItem2 hash tx itemKids)
        valueMaybe
  where
    mkItem2 :: HashStr -> TxSum -> Array (MenuTree Route) -> MenuTree Route
    mkItem2 hash tx itemKids = evalTxSum
      (\x -> mkItem ("☁️ "  <> shortHash hash)
                    (Just $ UberRootR Ex.endpointUrl)
                    :< itemKids
      )
      (\x -> mkItem ("🌐 "  <> shortHash hash)
                    (Just $ NamespaceR x.root.message)
                    :< itemKids
      )
      (\w -> mkItem ("🥨 " <> shortHash hash)
                    (Just $ WiringR { name: hash, endpointUrl: Ex.endpointUrl, hash: hash })
                    :< (fromNets w.wiring.nets <> fromDiagrams w.wiring.diagrams <> itemKids)
      )
      (\f -> mkItem ("🔥 " <> shortHash hash)
                    (Just $ FiringR { name: hash, endpointUrl: Ex.endpointUrl, hash: hash })
                    :< itemKids
      ) tx
      where
        fromNets     nets  = mapWithIndex (\ix n -> mkItem ("🔗 " <> n.name) (Just $ NetR     hash ix n.name) :< []) nets
        fromDiagrams diags = mapWithIndex (\ix d -> mkItem ("⛓ " <> d.name) (Just $ DiagramR hash ix d.name) :< []) diags

    mkUnloadedItem :: Array (MenuTree Route) -> MenuTree Route
    mkUnloadedItem itemKids = mkItem ("👻 " <> shortHash hash) unloadedRoute :< itemKids
      where
        -- TODO we need to return a Route currently, but we may want to return a (LoadTransaction ... ::Query) instead,
        -- so we could load unloaded hashes from the menu.
        unloadedRoute = Nothing

--------------------------------------------------------------------------------

shortHash :: HashStr -> String
shortHash = take 8
