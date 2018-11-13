module PetrinetView where

import Prelude
import Config
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map as Map
import Data.Monoid (guard)
import Data.Monoid.Additive (Additive(..))
import Data.Tuple (Tuple(..), uncurry, snd)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Vec2D (Vec2D)
import Data.Vec2D (bounds) as Vec2D
import Effect.Aff.Class (liftAff)
import Effect.Aff (Aff(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Core as Core
import Halogen.HTML (HTML, div)
import Halogen.HTML.Events as HE
import Svg.Elements as SE
import Svg.Attributes as SA
import Svg.Attributes (Duration, DurationF(..), seconds, FillState(Freeze), FontSize(..), CSSLength(..))
import Svg.Util as SvgUtil

import ExampleData as Ex
import ExampleData as Net
import Data.Petrinet.Representation.Dict
import ExampleData (PID, TID, Tokens, NetObj, NetApi)

data QueryF pid tid a
  = FireTransition tid a
  | MisfireTransition tid a -- ^ for inactive transitions

-- | Messages sent to the outside world (i.e. parent components).
--   TODO This is a dummy placeholder for now.
data Msg = NetUpdated

type StateF pid tid =
  { net    :: NetObjF pid tid Tokens
  , netApi :: NetApiF pid tid Tokens
  , msg    :: String
  }

type PlaceModelF pid tok label pt =
  { id     :: pid
  , tokens :: tok
  , label  :: label
  , point  :: pt
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

ui :: ∀ pid tid g. Ord pid => Show pid => Show tid => Maybe HtmlId -> StateF pid tid -> H.Component HTML (QueryF pid tid) Unit Msg Aff
ui htmlIdPrefixMaybe initialState =
  H.component { initialState: const initialState, render, eval, receiver: const Nothing }
  where
    render :: StateF pid tid -> HTML Void (QueryF pid tid Unit)
    render state =
      div [ HP.classes [ ClassName "petrinet-component" ] ]
          [ div [] [ HH.text state.msg ]
          , SE.svg [ SA.viewBox sceneLeft sceneTop sceneWidth sceneHeight ]
                   (netToSVG state.net)
          ]
      where
        sceneWidth  = (bounds.max.x - bounds.min.x) + paddingX
        sceneHeight = (bounds.max.y - bounds.min.y) + paddingY
        sceneLeft   = bounds.min.x - (paddingX / 2.0)
        sceneTop    = bounds.min.y - (paddingY / 2.0)
        bounds      = Vec2D.bounds (Map.values state.net.placePointsDict <> Map.values state.net.transitionPointsDict)
        paddingX    = 4.0 * transitionWidth -- TODO maybe stick the padding inside the bounding box?
        paddingY    = 4.0 * transitionHeight

    eval :: ∀ tid. Show tid => QueryF pid tid ~> H.ComponentDSL (StateF pid tid) (QueryF pid tid) Msg Aff
    eval = case _ of
      MisfireTransition tid next -> pure next
      FireTransition    tid next -> do
        numElems <- H.liftAff $ SvgUtil.beginElements ("." <> arcAnimationClass tid)
        H.modify_ (mod1 tid)
        H.raise NetUpdated -- notify parent components
        pure next
        where
          mod1 tid state =
            state { net = net'
                  , msg = "marking = " <> show (_.marking <$> netMaybe')
                  }
            where
              netMaybe' = fire state.net <$> state.net.findTransition tid
              net'      = fromMaybe state.net $ netMaybe'

    netToSVG :: ∀ tid a. Ord pid => Show pid => Show tid => NetObjF pid tid Tokens -> Array (HTML a ((QueryF pid tid) Unit))
    netToSVG net =
      svgTransitions <> svgPlaces
      where
        svgTransitions = fromMaybe [] $ traverse (uncurry drawTransitionAndArcs) $ Map.toUnfoldable $ net.transitionsDict

        svgPlaces = fromMaybe [] $ drawPlace1 `traverse` net.places

        -- TODO the do-block will fail as a whole if e.g. one findPLacePoint misses
        drawPlace1 :: pid -> Maybe (HTML a ((QueryF pid tid) Unit))
        drawPlace1 id = do
          label <- Map.lookup id net.placeLabelsDict
          point <- net.findPlacePoint id
          let tokens = findTokens net id
          pure $ svgPlace { id: id, tokens: tokens, label: label, point: point }

        -- TODO the do-block will fail as a whole if e.g. one findPlacePoint misses
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
                 , HE.onClick (HE.input_ (if isEnabled then FireTransition tid else MisfireTransition tid))
                 ]
                 (svgPreArcs <> svgPostArcs <> [svgTransitionRect trPos tid])

        -- TODO simplify, especially (src, dest) order given isPost
        mkPostArc :: ∀ tid a. Show tid => tid -> Vec2D -> PlaceMarkingF pid Tokens -> Maybe (ArcModel tid)
        mkPostArc tid src tp = { isPost: true, tid: tid, src: src, dest: _, label: postArcId tid tp.place, htmlId: postArcId tid tp.place } <$> net.findPlacePoint tp.place

        mkPreArc :: ∀ tid a. Show tid => tid -> Vec2D -> PlaceMarkingF pid Tokens -> Maybe (ArcModel tid)
        mkPreArc tid dest tp = { isPost: false, tid: tid, src: _, dest: dest, label: preArcId tid tp.place, htmlId: preArcId tid tp.place } <$> net.findPlacePoint tp.place

    --------------------------------------------------------------------------------

    svgTransitionRect :: ∀ a tid. Show tid => Vec2D -> tid -> HTML a ((QueryF pid tid) Unit)
    svgTransitionRect pos tid = SE.rect
      [ SA.class_  "css-transition-rect"
      , SA.width   transitionWidth
      , SA.height  transitionHeight
      , SA.x       (pos.x - transitionWidth / 2.0)
      , SA.y       (pos.y - transitionHeight / 2.0)
      ]

    svgArc :: ∀ a pid tid. Show tid => ArcModel tid -> HTML a ((QueryF pid tid) Unit)
    svgArc arc =
      SE.g [ SA.class_ "css-arc-container" ]
           [ SE.path
               [ SA.class_ $ "css-arc " <> if arc.isPost then "css-post-arc" else "css-pre-arc"
               , SA.id arc.htmlId -- we refer to this as the path of our animation and label, among others
               , svgPath arc.src arc.dest
               ]
           , SE.circle
               [ SA.class_ "css-arc-head" -- TODO yah, this should be a triangle
               , SA.cx      arc.dest.x
               , SA.cy      arc.dest.y
               , SA.r       1.5
               ]
           , SE.text
               [ SA.class_    "css-arc-label"
               , SA.x         arc.src.x
               , SA.y         arc.src.y
               , SA.font_size (FontSizeLength $ Px 2.0)
                 -- TODO add SVG.textPath prop, refer to the svg path using xlink:href="#<arc id here>"
               ]
               [ HH.text arc.htmlId ]
           , svgTokenAnimated arc
           ]

    -- | A token that moves along the path of the enclosing arc. This should happen
    --   when the transition to which this arc is connected fires.
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
    svgPlace { id: id, label: label, point: point, tokens: tokens } =
      SE.g [ SA.id (mkPlaceIdStr id) ]
           [ SE.title [] [ Core.text label ]
           , SE.circle
               [ SA.class_ "css-place"
               , SA.r      placeRadius
               , SA.cx     point.x
               , SA.cy     point.y
               ]
           , svgTokens tokens point
           , SE.text [ SA.class_    "css-place-label"
                     , SA.x         (point.x + 0.2 * placeRadius)
                     , SA.y         (point.y - 0.2 * placeRadius)
                     , SA.font_size (SA.FontSizeLength (SA.Px 2.0))
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


--------------------------------------------------------------------------------

svgPath :: ∀ r i. Vec2D -> Vec2D -> HP.IProp (d :: String | r) i
svgPath p q = SA.d $ SA.Abs <$> [ SA.M p.x p.y, SA.L q.x q.y ]
