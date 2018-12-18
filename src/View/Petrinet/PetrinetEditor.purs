module View.Petrinet.PetrinetEditor where

import Prelude
import Control.MonadZero (empty)
import Data.Array (catMaybes)
import Data.Newtype (un)
import Data.Bag (BagF)
import Data.Foldable (class Foldable, fold, foldMap, elem, intercalate)
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map as Map
import Data.Monoid (guard)
import Data.Monoid.Additive (Additive(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), uncurry, snd)
import Data.Tuple.Nested ((/\))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Vec2D (Vec2D)
import Data.Vec2D (bounds) as Vec2D
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (Aff(..))
import Halogen as H
import Halogen (ComponentDSL)
import Halogen.HTML as HH
import Halogen.HTML (HTML, div, br)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Core as Core
import Halogen.HTML.Events as HE
import Svg.Elements as SE
import Svg.Attributes as SA
import Svg.Attributes (Duration, DurationF(..), seconds, FillState(Freeze), FontSize(..), CSSLength(..))
import Svg.Util as SvgUtil

import Data.Auth
import Data.Petrinet.Representation.Dict
import ExampleData as Ex
import ExampleData as Net
import View.Common (HtmlId, emptyHtml)
import View.Petrinet.Arrow
import View.Petrinet.Arrow as Arrow
import View.Petrinet.Config
import View.Petrinet.Model (PID, TID, TextBoxId, Tokens, Typedef(..), NetObj, NetApi, NetInfoFRow, NetInfoF, QueryF(..), PlaceQueryF(..), TransitionQueryF(..), Msg(..), NetElemKind(..))
import View.Petrinet.PlaceEditor as PlaceEditor
import View.Petrinet.TransitionEditor as TransitionEditor

type StateF pid tid =
  { msg                     :: String
  , netInfo                 :: NetInfoF pid tid ()
  , focusedPlace            :: Maybe pid
  , focusedTransition       :: Maybe tid
  , arcLabelsVisible        :: Boolean
  , placeLabelsVisible      :: Boolean
  , transitionLabelsVisible :: Boolean
  }

type PlaceModelF pid tok label pt =
  { id        :: pid
  , tokens    :: tok
  , label     :: label
  , point     :: pt
  , isFocused :: Boolean
  }

-- TODO unify with TransitionEditor.TransitionEditorFormModel?
type TransitionModelF tid label pt =
  { id        :: tid
  , preArcs   :: Array (ArcModelF tid label pt)
  , postArcs  :: Array (ArcModelF tid label pt)
  , isEnabled :: Boolean
  , label     :: label
  , htmlId    :: HtmlId
  , isFocused :: Boolean
  , point     :: Vec2D
  , auths     :: Roles
  }

type ArcModelF tid label pt =
  { src    :: pt
  , dest   :: pt
  , label  :: label -- TODO String?
  , tid    :: tid
  , isPost :: Boolean
  , htmlId :: HtmlId
  }

-- TODO drat, we didn't want type parameters here
type ArcModel tid = ArcModelF tid String Vec2D

--------------------------------------------------------------------------------

ui :: ∀ pid tid m. MonadAff m => Ord pid => Show pid => Ord tid => Show tid => Array RoleInfo -> NetInfoF pid tid () -> H.Component HTML (QueryF pid tid) Unit Msg m
ui allRoleInfos initialNetInfo =
  H.component { initialState: const initialState, render, eval, receiver: const Nothing }
  where
    -- TODO should come from component state
    htmlIdPrefixMaybe = Just "todo_net_prefix"

    initialState :: StateF pid tid
    initialState =
      { netInfo:                 initialNetInfo
      , msg:                     "Please select a net."
      , focusedPlace:            empty
      , focusedTransition:       empty
      , placeLabelsVisible:      true
      , transitionLabelsVisible: true
      , arcLabelsVisible:        false
      }

    render :: StateF pid tid -> HTML Void (QueryF pid tid Unit)
    render state =
      div [ HP.id_ componentHtmlId
          , classes [ componentClass, ClassName "css-petrinet-component", ClassName $ arcLabelsVisibilityClass <> " " <> transitionLabelsVisibilityClass <> " " <> placeLabelsVisibilityClass ]
          ]
          [ SE.svg [ SA.viewBox sceneLeft sceneTop sceneWidth sceneHeight ]
                   (netToSVG state.netInfo.net state.focusedPlace state.focusedTransition)
          , HH.text state.msg
          , HH.br []
          , HH.br []
          , div [ classes [ ClassName "columns" ] ]
                [ div [ classes [ ClassName "column" ] ]
                      [ htmlMarking state.netInfo.net.marking ]
                , div [ classes [ ClassName "column" ] ]
                      [ maybe emptyHtml (map UpdatePlace <<< PlaceEditor.form <<< Just) do
                          pid <- state.focusedPlace
                          label <- Map.lookup pid state.netInfo.net.placeLabelsDict
                          pure { pid: pid, label: label, typedef: Typedef "Unit", isWriteable: false }
                      , maybe emptyHtml (map UpdateTransition <<< TransitionEditor.form' allRoleInfos) do
                          tid   <- state.focusedTransition
                          label <- Map.lookup tid state.netInfo.net.transitionLabelsDict
                          typ   <- Map.lookup tid state.netInfo.net.transitionTypesDict
                          let auths = fromMaybe mempty (Map.lookup tid state.netInfo.net.transitionAuthsDict)
                          pure { tid: tid, label: label, typedef: typ, isWriteable: false, auths: auths }
                      ]
                , div [ classes [ ClassName "column "] ]
                      [ labelVisibilityButtons ]
                ]
          ]
      where
        sceneWidth                      = (bounds.max.x - bounds.min.x) + paddingX
        sceneHeight                     = (bounds.max.y - bounds.min.y) + paddingY
        sceneLeft                       = bounds.min.x - (paddingX / 2.0)
        sceneTop                        = bounds.min.y - (paddingY / 2.0)
        bounds                          = Vec2D.bounds (Map.values state.netInfo.net.placePointsDict <> Map.values state.netInfo.net.transitionPointsDict)
        paddingX                        = 4.0 * transitionWidth -- TODO maybe stick the padding inside the bounding box?
        paddingY                        = 4.0 * transitionHeight
        arcLabelsVisibilityClass        = guard (not state.arcLabelsVisible)        "css-hide-arc-labels"
        placeLabelsVisibilityClass      = guard (not state.placeLabelsVisible)      "css-hide-place-labels"
        transitionLabelsVisibilityClass = guard (not state.transitionLabelsVisible) "css-hide-transition-labels"

    eval :: ∀ tid. Ord tid => Show tid => QueryF pid tid ~> ComponentDSL (StateF pid tid) (QueryF pid tid) Msg m
    eval = case _ of
      LoadNet newNetInfo next -> do
        H.modify_ (\state -> state { netInfo = newNetInfo
                                   , msg = "Select places or transitions by clicking on them. Double-click enabled transitions to fire them."
                                   })
        pure next
      FocusPlace pid next -> do
        state <- H.get
        let focusedPlace' = toggleMaybe pid state.focusedPlace
        H.put $ state { focusedPlace = focusedPlace'
                      , msg = (maybe "Focused" (const "Unfocused") state.focusedPlace) <>" place " <> show pid <> " (" <> (fold $ Map.lookup pid state.netInfo.net.placeLabelsDict) <> ")."
                      }
        pure next
      UpdatePlace (UpdatePlaceLabel pid newLabel next) -> do
        H.modify_ $ \state -> state { netInfo = state.netInfo { net = state.netInfo.net {  placeLabelsDict = Map.insert pid newLabel state.netInfo.net.placeLabelsDict } }
                                    , msg = "Updated place " <> show pid <> "."
                                    }
        pure next
      UpdateTransition (UpdateTransitionName tid newLabel next) -> do
        H.modify_ $ \state -> state { netInfo = state.netInfo { net = state.netInfo.net { transitionLabelsDict = Map.insert tid newLabel state.netInfo.net.transitionLabelsDict } }
                                    , msg = "Updated transition " <> show tid <> "."
                                    }
        pure next
      UpdateTransition (UpdateTransitionType tid newType next) -> do
        H.modify_ $ \state -> state { netInfo = state.netInfo { net = state.netInfo.net { transitionTypesDict = Map.insert tid newType state.netInfo.net.transitionTypesDict } }
                                    , msg = "Updated transition " <> show tid <> "."
                                    }
        pure next
      FocusTransition tid next -> do
        state <- H.get
        let focusedTransition' = toggleMaybe tid state.focusedTransition
        H.put $ state { focusedTransition = focusedTransition'
                      , msg = (maybe "Focused" (const "Unfocused") state.focusedTransition) <> " transition " <> show tid <> " (" <> (fold $ Map.lookup tid state.netInfo.net.transitionLabelsDict) <> ")."
                      }
        pure next
      FireTransition tid next -> do
        numElems <- H.liftAff $ SvgUtil.beginElements ("#" <> componentHtmlId <> " ." <> arcAnimationClass tid)
        state <- H.get
        let
          netMaybe' = fire state.netInfo.net <$> state.netInfo.net.findTransition tid
          net'      = fromMaybe state.netInfo.net netMaybe'
        H.put $ state { netInfo = state.netInfo { net = net'}
                      , msg = "Fired transition " <> show tid <> " (" <> (fold $ Map.lookup tid net'.transitionLabelsDict) <> ")."
                      }
        pure next
      ToggleLabelVisibility obj next -> do
        state <- H.get
        H.put $ case obj of
          Arc ->        state { arcLabelsVisible        = not state.arcLabelsVisible }
          Place ->      state { placeLabelsVisible      = not state.placeLabelsVisible }
          Transition -> state { transitionLabelsVisible = not state.transitionLabelsVisible }
        pure next


    netToSVG :: ∀ tid a. Ord pid => Show pid => Ord tid => Show tid => NetObjF pid tid Tokens Typedef -> Maybe pid -> Maybe tid -> Array (HTML a ((QueryF pid tid) Unit))
    netToSVG net focusedPlace focusedTransition =
      svgDefs <> svgTransitions <> svgPlaces
      where
        svgDefs        = [ SE.defs [] [ Arrow.svgArrowheadMarker ] ]
        svgTransitions = catMaybes $ map (map svgTransitionAndArcs <<< uncurry mkTransitionAndArcsModel) $ Map.toUnfoldable $ net.transitionsDict
        svgPlaces      = catMaybes $ (map svgPlace <<< mkPlaceModel) <$> net.places

        mkPlaceModel :: pid -> Maybe (PlaceModelF pid Tokens String Vec2D)
        mkPlaceModel id = do
          label <- Map.lookup id net.placeLabelsDict
          point <- net.findPlacePoint id
          let tokens = findTokens net id
          pure $ { id: id, tokens: tokens, label: label, point: point, isFocused: id `elem` focusedPlace }

        -- TODO the do-block will fail as a whole if e.g. one findPlacePoint misses
        mkTransitionAndArcsModel :: tid -> TransitionF pid Tokens -> Maybe (TransitionModelF tid String Vec2D)
        mkTransitionAndArcsModel tid tr = do
          trPoint  <- net.findTransitionPoint tid
          preArcs  <- mkPreArc  tid trPoint `traverse` tr.pre
          postArcs <- mkPostArc tid trPoint `traverse` tr.post
          let auths = fromMaybe (Roles mempty) (Map.lookup tid net.transitionAuthsDict)
          pure { id        : tid
               , label     : fold (Map.lookup tid net.transitionLabelsDict)
               , isEnabled : isTransitionEnabled net.marking tr
               , preArcs   : preArcs
               , postArcs  : postArcs
               , auths     : auths
               , htmlId    : mkTransitionIdStr tid
               , point     : trPoint
               , isFocused : focusedTransition == Just tid
               }
          where
            mkPostArc :: ∀ tid a. Show tid => tid -> Vec2D -> PlaceMarkingF pid Tokens -> Maybe (ArcModel tid)
            mkPostArc tid src tp = { isPost: true, tid: tid, src: src, dest: _, label: postArcId tid tp.place, htmlId: postArcId tid tp.place } <$> net.findPlacePoint tp.place

            mkPreArc :: ∀ tid a. Show tid => tid -> Vec2D -> PlaceMarkingF pid Tokens -> Maybe (ArcModel tid)
            mkPreArc tid dest tp = { isPost: false, tid: tid, src: _, dest: dest, label: preArcId tid tp.place, htmlId: preArcId tid tp.place } <$> net.findPlacePoint tp.place

    --------------------------------------------------------------------------------

    svgTransitionAndArcs :: ∀ tid a. Show tid => TransitionModelF tid String Vec2D -> HTML a ((QueryF pid tid) Unit)
    svgTransitionAndArcs t =
      SE.g [ SA.class_ $ "css-transition" <> (guard t.isEnabled " enabled") <> " " <> intercalate " " roleClasses
           , SA.id t.htmlId
           , HE.onClick (HE.input_ (FocusTransition t.id))
           , HE.onDoubleClick (HE.input_ (if t.isEnabled then FireTransition t.id else FocusTransition t.id))
           ]
           ((svgArc <$> (t.preArcs <> t.postArcs)) <> [svgTransitionRect t] <> [svgTransitionLabel t])
           where
             roleClasses :: Array String
             roleClasses = map (\r -> "css-role-" <> show r) <<< Set.toUnfoldable <<< un Roles $ t.auths

    svgTransitionRect :: ∀ tid a. Show tid => TransitionModelF tid String Vec2D -> HTML a ((QueryF pid tid) Unit)
    svgTransitionRect t =
      SE.rect [ SA.class_  ("css-transition-rect" <> guard t.isFocused " focused")
              , SA.width   transitionWidth
              , SA.height  transitionHeight
              , SA.x       (t.point.x - transitionWidth / 2.0)
              , SA.y       (t.point.y - transitionHeight / 2.0)
              ]

    svgTransitionLabel :: ∀ tid a. Show tid => TransitionModelF tid String Vec2D -> HTML a ((QueryF pid tid) Unit)
    svgTransitionLabel t =
      SE.text [ SA.class_    "css-transition-name-label"
              , SA.x         (t.point.x + 1.5 * placeRadius)
              , SA.y         (t.point.y + 4.0 * fontSize)
              , SA.font_size (SA.FontSizeLength $ Em fontSize)
              ]
              [ HH.text t.label ]

    svgArc :: ∀ pid tid a. Show tid => ArcModel tid -> HTML a ((QueryF pid tid) Unit)
    svgArc arc =
      SE.g [ SA.class_ "css-arc-container" ]
           [ SE.path
               [ SA.class_ $ "css-arc " <> if arc.isPost then "css-post-arc" else "css-pre-arc"
               , SA.id arc.htmlId -- we refer to this as the path of our animation and label, among others
               , SA.d (svgPath arc.src arc.dest)
               ]
           , svgArrow arc.src arc.dest
           , SE.text
               [ SA.class_    "css-arc-name-label"
               , SA.x         arc.src.x
               , SA.y         arc.src.y
               , SA.font_size (FontSizeLength $ Em fontSize)
                 -- TODO add SVG.textPath prop, refer to the svg path using xlink:href="#<arc id here>"
               ]
               [ HH.text arc.htmlId ]
           , svgTokenAnimated arc
           ]

    -- | A token that moves along the path of the enclosing arc. This should happen
    -- | when the transition to which this arc is connected fires.
    svgTokenAnimated :: ∀ pid tid a. Show tid => ArcModel tid -> HTML a ((QueryF pid tid) Unit)
    svgTokenAnimated arc =
      SE.circleNode
        [ SA.class_ "css-token-animated"
        , SA.cx      0.0
        , SA.cy      0.0
        , SA.r       0.0
        ]
        [ SE.animateMotion
            [ SA.id          animationId
            , SA.class_      (arcAnimationClass arc.tid) -- used to trigger the animation with beginElement
            , SA.begin       "indefinite"
            , SA.dur         arcAnimationDuration
            , SA.repeatCount 1
            -- TODO we reproduce the path here as a workaround because the mpath with the xlink:href doesn't work yet
            , SA.path        (svgPath arc.src arc.dest)
            ]
            [ -- TODO doesn't work, possibly due to xlink attribute namespace issue
              -- SE.mpath [ SA.xlinkHref $ "#" <> arc.htmlId ] -- token will move along this referenced path
            ]
        , SE.animate
            [ SA.attributeName "opacity"
            , SA.from          "0"
            , SA.to            "1"
            , SA.begin         (animationId <> ".begin")
            , SA.dur           tokenFadeDuration
            , SA.fillAnim      Freeze
            , SA.repeatCount   0
            ]
        , SE.animate
            [ SA.attributeName "r"
            , SA.from          (show 0.0)
            , SA.to            (show $ 1.5 * tokenRadius)
            , SA.begin         (animationId <> ".begin")
            , SA.dur           tokenFadeDuration
            , SA.fillAnim      Freeze
            , SA.repeatCount   0
            ]
        ]
      where
        animationId = tokenAnimatedClass arc.htmlId
        tokenFadeDuration = arcAnimationDuration <#> (_ / 4.0)

    --------------------------------------------------------------------------------

    svgPlace :: ∀ pid tid a. Show pid => PlaceModelF pid Tokens String Vec2D -> HTML a ((QueryF pid tid) Unit)
    svgPlace { id: id, label: label, point: point, tokens: tokens, isFocused: isFocused } =
      SE.g [ SA.id (mkPlaceIdStr id)
           , HE.onClick (HE.input_ (FocusPlace id))
           ]
           [ SE.title [] [ Core.text label ]
           , SE.circle
               [ SA.class_ ("css-place" <> guard isFocused " focused")
               , SA.r      placeRadius
               , SA.cx     point.x
               , SA.cy     point.y
               ]
           , svgTokens tokens point
           , SE.text [ SA.class_    "css-place-name-label"
                     , SA.x         (point.x + placeRadius + placeRadius / 2.0)
                     , SA.y         (point.y + 4.0 * fontSize)
                     , SA.font_size (SA.FontSizeLength $ Em fontSize)
                     ]
                     [ HH.text label ]
           , SE.text [ SA.class_    "css-place-label"
                     , SA.x         (point.x + tokenPadding)
                     , SA.y         (point.y - tokenPadding)
                     , SA.font_size (SA.FontSizeLength $ Em fontSize)
                     ]
                     [ HH.text $ if tokens == 0 || tokens == 1 then "" else show tokens ]
           ]
      where
        svgTokens :: Tokens -> Vec2D -> HTML a ((QueryF pid tid) Unit)
        svgTokens tokens point = if Additive tokens == mempty then HH.text "" else
          SE.circle
            [ SA.r      tokenRadius
            , SA.cx     point.x
            , SA.cy     point.y
            , SA.class_ "css-token-in-place"
            ]

    --------------------------------------------------------------------------------

    componentClass :: ClassName
    componentClass = ClassName "petrinet-component"

    componentHtmlId :: HtmlId
    componentHtmlId = netPrefix <> un ClassName componentClass

    -- | Provide distinct id's for distinct nets in a webpage.
    netPrefix :: String
    netPrefix = foldMap (_ <> "_") htmlIdPrefixMaybe

    prefixTransition :: ∀ tid. Show tid => tid -> HtmlId
    prefixTransition tid = "t" <> show tid

    prefixPlace :: ∀ pid. Show pid => pid -> HtmlId
    prefixPlace pid = "p" <> show pid

    postArcId :: ∀ pid tid. Show pid => Show tid => tid -> pid -> HtmlId
    postArcId tid place = netPrefix <> "arc_" <> prefixTransition tid <> "_" <> prefixPlace place

    preArcId :: ∀ pid tid. Show pid => Show tid => tid -> pid -> HtmlId
    preArcId tid place = netPrefix <> "arc_" <> prefixPlace place <> "_" <> prefixTransition tid

    arcAnimationClass :: ∀ tid. Show tid => tid -> HtmlId
    arcAnimationClass = append netPrefix <<< tokenAnimatedClass <<< mkArcClass
      where
        mkArcClass :: ∀ tid. Show tid => tid -> HtmlId
        mkArcClass tid = "arc_" <> prefixTransition tid

    tokenAnimatedClass x = x <> "_token_animated"

    mkTransitionIdStr :: ∀ tid. Show tid => tid -> HtmlId
    mkTransitionIdStr = append netPrefix <<< prefixTransition

    mkPlaceIdStr :: ∀ pid. Show pid => pid -> HtmlId
    mkPlaceIdStr = append netPrefix <<< prefixPlace

--------------------------------------------------------------------------------

htmlMarking :: ∀ a n pid tid. Show a => Show n => BagF a n -> HTML Void (QueryF pid tid Unit)
htmlMarking bag =
  HH.table [ classes [ ClassName "table", ClassName "is-striped", ClassName "is-narrow", ClassName "is-hoverable" ] ]
           [ HH.thead []
                      [ HH.tr [] [ HH.th [] [ HH.text "place" ]
                                 , HH.th [] [ HH.text "tokens" ]
                                 ]
                      ]
           , HH.tbody [] rows
           ]
  where
    rows = map (uncurry tr) <<< Map.toUnfoldable <<< unMarkingF $ bag
    tr k v = HH.tr [] [ HH.td [] [ HH.text $ show k ]
                      , HH.td [] [ HH.text $ show v ]
                      ]

--------------------------------------------------------------------------------

labelVisibilityButtons :: ∀ pid tid. HTML Void (QueryF tid pid Unit)
labelVisibilityButtons =
  div [ classes [ ClassName "field has-addons" ] ]
      [ HH.p [ classes [ ClassName "control" ] ]
             [ HH.a [ classes [ ClassName "button", ClassName "is-small" ]
                    , HE.onClick $ HE.input_ $ ToggleLabelVisibility Place ]
                    [ HH.span [] [ HH.text "Place labels" ] ]
             ]
      , HH.p [ classes [ ClassName "control" ] ]
             [ HH.a [ classes [ ClassName "button", ClassName "is-small" ]
                    , HE.onClick $ HE.input_ $ ToggleLabelVisibility Transition ]
                    [ HH.span [] [ HH.text "Transition labels" ] ]
             ]
      , HH.p [ classes [ ClassName "control" ] ]
             [ HH.a [ classes [ ClassName "button", ClassName "is-small" ]
                    , HE.onClick $ HE.input_ $ ToggleLabelVisibility Arc ]
                    [ HH.span [] [ HH.text "Arc labels" ] ]
             ]
      ]

--------------------------------------------------------------------------------

svgPath :: ∀ r i. Vec2D -> Vec2D -> Array SA.D
svgPath p q = SA.Abs <$> [ SA.M p.x p.y, SA.L q.x q.y ]

--------------------------------------------------------------------------------

toggleMaybe :: ∀ a b. b -> Maybe a -> Maybe b
toggleMaybe z mx = case mx of
  Nothing -> Just z
  Just _  -> Nothing
