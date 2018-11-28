module PetrinetView where

import Prelude
import Config
import Control.MonadZero (empty)
import Data.Array (catMaybes)
import Data.Newtype (un)
import Data.Bag (BagF)
import Data.Foldable (class Foldable, foldMap, elem)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map as Map
import Data.Monoid (guard)
import Data.Monoid.Additive (Additive(..))
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
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Core as Core
import Halogen.HTML (HTML, div)
import Halogen.HTML.Events as HE
import Svg.Elements as SE
import Svg.Attributes as SA
import Svg.Attributes (Duration, DurationF(..), seconds, FillState(Freeze), FontSize(..), CSSLength(..))
import Svg.Util as SvgUtil

import Arrow
import Arrow as Arrow
import ExampleData as Ex
import ExampleData as Net
import Data.Petrinet.Representation.Dict
import Model (PID, TID, Tokens, Typedef(..), NetObj, NetApi, NetInfoFRow, NetInfoF, QueryF(..), PlaceQueryF(..), TransitionQueryF(..), Msg(..))
import PlaceEditor as PlaceEditor
import TransitionEditor as TransitionEditor

type StateF pid tid =
  { focusedPlace      :: Maybe pid
  , focusedTransition :: Maybe tid
  , msg               :: String
  |                      NetInfoFRow pid tid ()
  }

type PlaceModelF pid tok label pt =
  { id        :: pid
  , tokens    :: tok
  , label     :: label
  , point     :: pt
  , isFocused :: Boolean
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

type HtmlId = String

--------------------------------------------------------------------------------

ui :: ∀ m pid tid r. MonadAff m => Ord pid => Show pid => Ord tid => Show tid => NetInfoF pid tid r -> H.Component HTML (QueryF pid tid) Unit Msg m
ui initialState' =
  H.component { initialState: const initialState, render, eval, receiver: const Nothing }
  where
    -- TODO should come from component state
    htmlIdPrefixMaybe = Just "todo_net_prefix"

    initialState :: StateF pid tid
    initialState =
      { name:              ""
      , net:               initialState'.net
      , netApi:            initialState'.netApi
      , msg:               "Please select a net."
      , focusedPlace:      empty
      , focusedTransition: empty
      }

    render :: StateF pid tid -> HTML Void (QueryF pid tid Unit)
    render state =
      div [ HP.id_ componentHtmlId
          , classes [ componentClass, ClassName "css-petrinet-component" ]
          ]
          [ SE.svg [ SA.viewBox sceneLeft sceneTop sceneWidth sceneHeight ]
                   (netToSVG state.net state.focusedPlace state.focusedTransition)
          , HH.text state.msg
          , HH.br []
          , HH.br []
          , div [ classes [ ClassName "columns" ] ]
                [ div [ classes [ ClassName "column" ] ]
                      [ htmlMarking state.net.marking ]
                , div [ classes [ ClassName "column" ] ]
                      [ HH.h1 [ classes [ ClassName "title", ClassName "is-6" ] ] [ HH.text "edit place" ]
                      , map UpdatePlace <<< PlaceEditor.form $ do
                          pid <- state.focusedPlace
                          label <- Map.lookup pid state.net.placeLabelsDict
                          pure { pid: pid, label: label, typedef: Typedef "Unit", isWriteable: false }
                      ]
                , div [ classes [ ClassName "column" ] ]
                      [ HH.h1 [ classes [ ClassName "title", ClassName "is-6" ] ] [ HH.text "edit transition" ]
                      , map UpdateTransition <<< TransitionEditor.form $ do
                          tid   <- state.focusedTransition
                          label <- Map.lookup tid state.net.transitionLabelsDict
                          typ   <- Map.lookup tid state.net.transitionTypesDict
                          pure { tid: tid, label: label, typedef: typ, isWriteable: false }
                      ]
                ]
          ]
      where
        sceneWidth  = (bounds.max.x - bounds.min.x) + paddingX
        sceneHeight = (bounds.max.y - bounds.min.y) + paddingY
        sceneLeft   = bounds.min.x - (paddingX / 2.0)
        sceneTop    = bounds.min.y - (paddingY / 2.0)
        bounds      = Vec2D.bounds (Map.values state.net.placePointsDict <> Map.values state.net.transitionPointsDict)
        paddingX    = 4.0 * transitionWidth -- TODO maybe stick the padding inside the bounding box?
        paddingY    = 4.0 * transitionHeight

    eval :: ∀ tid. Ord tid => Show tid => QueryF pid tid ~> ComponentDSL (StateF pid tid) (QueryF pid tid) Msg m
    eval = case _ of
      LoadNet newNet next -> do
        H.modify_ (\state -> state { net = newNet
                                   , msg = "Select places or transitions by clicking on them. Double-click enabled transitions to fire them."
                                   })
        pure next
      FocusPlace pid next -> do
        state <- H.get
        let focusedPlace' = toggleMaybe pid state.focusedPlace
        H.put $ state { focusedPlace = focusedPlace'
                      , msg = (maybe "Focused" (const "Unfocused") state.focusedPlace) <>" place " <> show pid <> "."
                      }
        pure next
      UpdatePlace (UpdatePlaceLabel pid newLabel next) -> do
        H.modify_ $ \state -> state { net = state.net { placeLabelsDict = Map.insert pid newLabel state.net.placeLabelsDict }
                                    , msg = "Updated place " <> show pid <> "."
                                    }
        pure next
      UpdateTransition (UpdateTransitionName tid newLabel next) -> do
        H.modify_ $ \state -> state { net = state.net { transitionLabelsDict = Map.insert tid newLabel state.net.transitionLabelsDict }
                                    , msg = "Updated transition " <> show tid <> "."
                                    }
        pure next
      UpdateTransition (UpdateTransitionType tid newType next) -> do
        H.modify_ $ \state -> state { net = state.net { transitionTypesDict = Map.insert tid newType state.net.transitionTypesDict }
                                    , msg = "Updated transition " <> show tid <> "."
                                    }
        pure next
      FocusTransition tid next -> do
        state <- H.get
        let focusedTransition' = toggleMaybe tid state.focusedTransition
        H.put $ state { focusedTransition = focusedTransition'
                      , msg = (maybe "Focused" (const "Unfocused") state.focusedTransition) <>" transition " <> show tid <> "."
                      }
        pure next
      FireTransition tid next -> do
        numElems <- H.liftAff $ SvgUtil.beginElements ("#" <> componentHtmlId <> " ." <> arcAnimationClass tid)
        state <- H.get
        let
          netMaybe' = fire state.net <$> state.net.findTransition tid
          net'      = fromMaybe state.net netMaybe'
        H.put $ state { net = net'
                      , msg = "Fired transition " <> show tid <> "."
                      }
        pure next

    netToSVG :: ∀ tid a. Ord pid => Show pid => Show tid => NetObjF pid tid Tokens Typedef -> Maybe pid -> Maybe tid -> Array (HTML a ((QueryF pid tid) Unit))
    netToSVG net focusedPlace focusedTransition =
      svgDefs <> svgTransitions <> svgPlaces
      where
        svgDefs = [ SE.defs [] [ Arrow.svgArrowheadMarker ] ]

        svgTransitions = fromMaybe [] $ traverse (uncurry drawTransitionAndArcs) $ Map.toUnfoldable $ net.transitionsDict

        -- TODO catMaybes will cause this to fail silently on `Nothing`s
        svgPlaces = catMaybes $ (map svgPlace <<< mkPlaceModel) <$> net.places

        mkPlaceModel :: pid -> Maybe (PlaceModelF pid Tokens String Vec2D)
        mkPlaceModel id = do
          label <- Map.lookup id net.placeLabelsDict
          point <- net.findPlacePoint id
          let tokens = findTokens net id
          pure $ { id: id, tokens: tokens, label: label, point: point, isFocused: id `elem` focusedPlace }

        -- TODO the do-block will fail as a whole if e.g. one findPlacePoint misses
        -- | Arcs are contained within a transition in the generated SVG.
        drawTransitionAndArcs :: ∀ a. tid -> TransitionF pid Tokens -> Maybe (HTML a ((QueryF pid tid) Unit))
        drawTransitionAndArcs tid tr = do
          trPos <- net.findTransitionPoint tid

          preArcs   <- mkPreArc  tid trPos `traverse` tr.pre
          postArcs  <- mkPostArc tid trPos `traverse` tr.post

          let
            svgPreArcs  = svgArc <$> preArcs
            svgPostArcs = svgArc <$> postArcs
            isEnabled   = isTransitionEnabled net.marking tr

          pure $
            SE.g [ SA.class_ $ "css-transition" <> guard isEnabled " enabled"
                 , SA.id (mkTransitionIdStr tid)
                 , HE.onClick (HE.input_ (FocusTransition tid))
                 , HE.onDoubleClick (HE.input_ (if isEnabled then FireTransition tid else FocusTransition tid))
                 ]
                 (svgPreArcs <> svgPostArcs <> [svgTransitionRect trPos tid] <> [svgTransitionLabel trPos tid])

        -- TODO simplify, especially (src, dest) order given isPost
        mkPostArc :: ∀ tid a. Show tid => tid -> Vec2D -> PlaceMarkingF pid Tokens -> Maybe (ArcModel tid)
        mkPostArc tid src tp = { isPost: true, tid: tid, src: src, dest: _, label: postArcId tid tp.place, htmlId: postArcId tid tp.place } <$> net.findPlacePoint tp.place

        mkPreArc :: ∀ tid a. Show tid => tid -> Vec2D -> PlaceMarkingF pid Tokens -> Maybe (ArcModel tid)
        mkPreArc tid dest tp = { isPost: false, tid: tid, src: _, dest: dest, label: preArcId tid tp.place, htmlId: preArcId tid tp.place } <$> net.findPlacePoint tp.place

    --------------------------------------------------------------------------------

    svgTransitionRect :: ∀ a tid. Show tid => Vec2D -> tid -> HTML a ((QueryF pid tid) Unit)
    svgTransitionRect point tid =
      SE.rect [ SA.class_  "css-transition-rect"
              , SA.width   transitionWidth
              , SA.height  transitionHeight
              , SA.x       (point.x - transitionWidth / 2.0)
              , SA.y       (point.y - transitionHeight / 2.0)
              ]

    svgTransitionLabel :: ∀ a tid. Show tid => Vec2D -> tid -> HTML a ((QueryF pid tid) Unit)
    svgTransitionLabel point tid =
      SE.text [ SA.class_    "css-transition-name-label"
              , SA.x         (point.x + 1.5 * placeRadius)
              , SA.y         (point.y + 4.0 * fontSize)
              , SA.font_size (SA.FontSizeLength $ Em fontSize)
              ]
              [ HH.text $ mkTransitionIdStr tid ]

    svgArc :: ∀ a pid tid. Show tid => ArcModel tid -> HTML a ((QueryF pid tid) Unit)
    svgArc arc =
      SE.g [ SA.class_ "css-arc-container" ]
           [ SE.path
               [ SA.class_ $ "css-arc " <> if arc.isPost then "css-post-arc" else "css-pre-arc"
               , SA.id arc.htmlId -- we refer to this as the path of our animation and label, among others
               , svgPath arc.src arc.dest
               ]
           , svgArrow arc.src arc.dest
           , SE.text
               [ SA.class_    "css-arc-label"
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
    svgTokenAnimated :: ∀ a pid tid. Show tid => ArcModel tid -> HTML a ((QueryF pid tid) Unit)
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
            , SA.path        $ SA.Abs <$> [ SA.M arc.src.x arc.src.y, SA.L arc.dest.x arc.dest.y ]
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

    svgPlace :: ∀ a pid tid. Show pid => PlaceModelF pid Tokens String Vec2D -> HTML a ((QueryF pid tid) Unit)
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

svgPath :: ∀ r i. Vec2D -> Vec2D -> HP.IProp (d :: String | r) i
svgPath p q = SA.d $ SA.Abs <$> [ SA.M p.x p.y, SA.L q.x q.y ]

--------------------------------------------------------------------------------

toggleMaybe :: ∀ m a b. b -> Maybe a -> Maybe b
toggleMaybe z mx = case mx of
  Nothing -> Just z
  Just mx -> Nothing
