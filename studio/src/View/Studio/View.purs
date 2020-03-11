module View.Studio.View where

import Prelude hiding (div)
import Affjax (URL) -- TODO introduce URL alias in Client so we can abstract Affjax away
import Control.Comonad.Cofree ((:<))
import Data.AdjacencySpace as AdjacencySpace
import Data.AdjacencySpace (AdjacencySpace)
import Data.Array (cons)
import Data.Foldable (foldMap, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String.CodePoints (take)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen (ComponentHTML)
import Halogen.HTML (a, br, button, div, fieldset, hr, h1, img, input, legend, li, nav, ol, p, slot, span, text, ul)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (classes, src, href, placeholder, value, tabIndex, type_, InputType(InputText))

import Language.Statebox.Wiring.Generator.DiagramV2.Operators (fromOperators, toPixel) as DiagramV2
import TreeMenu as TreeMenu
import TreeMenu (mkItem, MenuTree, Item)
import Statebox.Core.Transaction (HashStr, TxSum, evalTxSum, isExecutionTx)
import Statebox.Core.Types (NetsAndDiagramsIndex(..))
import View.Auth.RolesEditor as RolesEditor
import View.Diagram.DiagramEditor as DiagramEditor
import View.Diagram.Model (DiagramInfo)
import View.Diagram.Update as DiagramEditor
import View.KDMonCat.App as KDMonCat.App
import View.KDMonCat.Bricks as KDMonCat.Bricks
import View.Model (Project, NetInfoWithTypesAndRoles)
import View.Petrinet.PetrinetEditor as PetrinetEditor
import View.Petrinet.Model as PetrinetEditor
import View.Studio.Model (Action(..), State, resolveRoute)
import View.Studio.Model.Route
import View.Transaction (firingTxView, wiringTxView)
import View.Typedefs.TypedefsEditor as TypedefsEditor

--------------------------------------------------------------------------------

type ChildSlots =
  ( objectTree     :: H.Slot VoidF (TreeMenu.Msg Route) Unit
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
    , div [ classes [ ClassName "has-columns" ] ]
          [ nav [ classes [ ClassName "stbx-sidebar" ] ]
                [ slot _objectTree unit (TreeMenu.menuComponent (_ == state.route)) (stateMenu state) ((\(TreeMenu.Clicked menuNodeId route) -> ShowDiagramNodeContent route) >>> Just) ]
          , div []
                [ div [ classes $ ClassName <$> [ "container", "is-fluid" ] ]
                      [ routeBreadcrumbs state.route
                      , resolveRoute state.route state # maybe (text "Couldn't find project/net/diagram.")
                                                               (contentView state.apiUrl)
                      ]
                ]
          ]
    ]

contentView :: ∀ m. MonadAff m => URL -> ResolvedRouteF Project DiagramInfo NetInfoWithTypesAndRoles -> ComponentHTML Action ChildSlots m
contentView apiUrl route = case route of
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
        [ p [] [ text $ "Project '" <> project.name <> "'" ]
        , p []
            [ button [ onClick \_ -> Just CreateKDMonCat ]
                     [ text "Create new KDMonCat diagram" ]
            ]
        ]

  ResolvedTypes project ->
    TypedefsEditor.typedefsTreeView project.types

  ResolvedAuths project ->
    RolesEditor.roleInfosHtml project.roleInfos

  ResolvedNet netInfo ->
    slot _petrinetEditor unit (PetrinetEditor.ui (Just "main_net")) netInfo (Just <<< HandlePetrinetEditorMsg)

  ResolvedDiagram diagramInfo nodeMaybe ->
    div [ classes [ ClassName "has-columns" ] ]
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

  ResolvedKDMonCat kdmoncatInput ->
     div [ classes [ ClassName "w-full", ClassName "pl-4" ] ]
         [ slot _kdmoncatApp unit KDMonCat.App.appView kdmoncatInput (Just <<< HandleKDMonCatAppMsg) ]

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
  nav [ classes [ ClassName "stbx-breadcrumbs" ] ]
      [ ol [] $
           crumb <$> case route of
                       Home                          -> [ "Home" ]
                       TxHome                        -> [ "Home" ]
                       ProjectRoute projectName pr   -> [ projectName ] <> projectRouteBreadcrumbs pr
                       ApiRoute     apiRoute         -> apiRouteBreadcrumbs apiRoute
      ]
  where
    crumb str = li [] [ a [ href "#" ] [ text str ] ]

    projectRouteBreadcrumbs :: ProjectRoute -> Array _
    projectRouteBreadcrumbs = case _ of
      ProjectHome       -> []
      Types             -> [ "Types" ]
      Auths             -> [ "Authorisation" ]
      Net        name   -> [ name ]
      Diagram    name _ -> [ name ]
      KDMonCatR  name   -> [ name ]

    apiRouteBreadcrumbs :: ApiRoute -> Array _
    apiRouteBreadcrumbs = case _ of
      UberRootR  url                -> [ "über-namespace", url ]
      NamespaceR hash               -> [ "namespace", shortHash hash ]
      WiringR    x                  -> [ x.endpointUrl, "wiring " <> shortHash x.hash ]
      FiringR    x                  -> [ x.endpointUrl, shortHash x.hash ]
      DiagramR   hash ix name       -> [ shortHash hash, "diagram " <> show ix <> " " <> name ]
      NetR       hash ix name       -> [ shortHash hash, "net "     <> show ix <> " " <> name ]

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

stateMenu :: State -> MenuTree Route
stateMenu { projects, apiUrl, hashSpace } =
  mkItem "Studio" Nothing :< (txItems <> projectItems)
  where
    txItems        = AdjacencySpace.unsafeToTree (transactionMenu apiUrl) hashSpace <$> Set.toUnfoldable (AdjacencySpace.rootKeys hashSpace)
    projectItems   = projectMenu <$> projects

projectMenu :: Project -> MenuTree Route
projectMenu p =
  mkItem p.name (Just $ ProjectRoute p.name $ ProjectHome) :<
    [ mkItem "Types"          (Just $ ProjectRoute p.name $ Types) :< []
    , mkItem "Authorisations" (Just $ ProjectRoute p.name $ Auths) :< []
    , mkItem "Nets"           (Nothing)             :< fromNets     p p.nets
    , mkItem "Diagrams"       (Nothing)             :< fromDiagrams p p.diagrams
    , mkItem "KDMonCats"      (Nothing)             :< fromKDMonCats  (p.kdmoncats # Map.toUnfoldable)
    ]
  where
    fromNets      p nets  = (\n            -> mkItem n.name (Just $ ProjectRoute p.name $ Net       n.name        ) :< []) <$> nets
    fromDiagrams  p diags = (\d            -> mkItem d.name (Just $ ProjectRoute p.name $ Diagram   d.name Nothing) :< []) <$> diags
    fromKDMonCats   diags = (\(dname /\ d) -> mkItem dname  (Just $ ProjectRoute p.name $ KDMonCatR dname         ) :< []) <$> diags

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
                    (Just $ ApiRoute $ UberRootR apiUrl)
                    :< itemKids
      )
      (\x -> mkItem ("🌐 "  <> shortHash hash)
                    (Just $ ApiRoute $ NamespaceR x.root.message)
                    :< itemKids
      )
      (\w -> mkItem ("🥨 " <> shortHash hash)
                    (Just $ ApiRoute $ WiringR { name: hash, endpointUrl: apiUrl, hash: hash })
                    :< (fromNets w.wiring.nets <> fromDiagrams w.wiring.diagrams <> itemKids)
      )
      (\f -> mkItem ((if isExecutionTx f then "🔫 " else "🔥 ") <> shortHash hash)
                    (Just $ ApiRoute $ FiringR { name: hash, endpointUrl: apiUrl, hash: hash })
                    :< (flattenTree =<< itemKids) -- for nested firings, just drop the 'flattenTree' part
      )
      tx
      where
        fromNets     nets  = mapWithIndex (\ix n -> mkItem ("🔗 " <> n.name) (Just $ ApiRoute $ NetR     hash (NetsAndDiagramsIndex ix) n.name) :< []) nets
        fromDiagrams diags = mapWithIndex (\ix d -> mkItem ("⛓ " <> d.name) (Just $ ApiRoute $ DiagramR hash (NetsAndDiagramsIndex ix) d.name) :< []) diags

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
        -- TODO we need to return an ApiRoute currently, but we may want to return a (LoadTransaction ... :: Query) instead,
        -- so we could load unloaded hashes from the menu.
        unloadedRoute = Nothing

--------------------------------------------------------------------------------

projectsDashboard :: ∀ m. Array Project -> ComponentHTML Action ChildSlots m
projectsDashboard projects =
  div []
      [ ul [] $
          li [] [ a [ href "#", onClick \_ -> Just CreateProject ]
                    [ text "Create new project" ]
                ]
          `cons` (projects <#> mkProjectLink)
        ]
  where
    mkProjectLink p =
      li []
         [ a [ href "#", onClick \_ -> Just (SelectRoute (ProjectRoute p.name ProjectHome)) ]
             [ text p.name ]
         , a [ href "#", onClick \_ -> Just (DeleteProject p.projectId) ]
             [ text "☠" ]
         ]

homeForm :: ∀ m. URL -> ComponentHTML Action ChildSlots m
homeForm apiUrl =
  div []
      [ fieldset []
                 [ legend [] [ text "Please select an object from the menu, or enter a transaction hash below." ]
                 , p []
                     [ input [ value ""
                             , type_ InputText
                             , placeholder "Enter Statebox Cloud transaction hash"
                             , onValueInput $ Just <<< LoadTransactions apiUrl
                             ]
                     ]
                 , p []
                     [ input [ value apiUrl
                             , type_ InputText
                             , placeholder "Statebox API URL"
                             , onValueInput $ Just <<< SetApiUrl
                             ]
                     ]
                 ]
               , fieldset []
                          [ legend [] [ text "Alternatively, you can load a PNPRO file." ]
                          , p []
                              [ input [ value ""
                                      , type_ InputText
                                      , placeholder "Enter http URL to PNPRO file"
                                      , onValueInput $ Just <<< LoadPNPRO
                                      ]
                              ]
                          ]
      ]

--------------------------------------------------------------------------------

shortHash :: HashStr -> String
shortHash = take 8
