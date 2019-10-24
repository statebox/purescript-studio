module View.Petrinet.PetrinetEditor where

import Prelude hiding (div)
import Control.MonadZero (empty)
import Data.Array (catMaybes, (..))
import Data.Newtype (un, unwrap)
import Data.Bag (BagF)
import Data.Foldable (fold, foldMap, elem, intercalate)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map as Map
import Data.Monoid (guard)
import Data.Set as Set
import Data.Tuple (uncurry)
import Data.Traversable (traverse)
import Data.Vec3 (Vec2D, vec2, _x, _y)
import Effect.Aff (delay, Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen (ComponentHTML, HalogenM, mkEval, defaultEval)
import Halogen.HTML as HH
import Halogen.HTML (HTML, div)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Core (ClassName(..), ElemName(..), AttrName(..))
import Halogen.HTML.Core as Core
import Halogen.HTML.Events as HE
import Math (sin, cos, pi)
import Svg.Elements as SE
import Svg.Attributes as SA
import Svg.Attributes (CSSLength(..), FillState(..), FontSize(..))
import Svg.Util as SvgUtil

import Data.Auth (Roles(..))
import Data.Petrinet.Representation.Dict (TransitionF, NetLayoutF, PlaceMarkingF, isTransitionEnabled, fire, preMarking, mkNetApiF)
import Data.Petrinet.Representation.Layout.Bipartite as Bipartite
import Data.Petrinet.Representation.Layout.Dagre as Dagre
import Data.Petrinet.Representation.Marking as Marking
import Data.Typedef (Typedef(..))

import View.Common (HtmlId, emptyHtml, mapAction)
import View.Petrinet.Arrow (svgArrow, svgArrowheadMarker)
import View.Petrinet.Config as Config
import View.Petrinet.Config (placeRadius, transitionWidth, transitionHeight, tokenRadius, tokenPadding, fontSize, arcAnimationDuration, arcAnimationDurationSec)
import View.Petrinet.Model as NetInfo -- TODO move the NetInfo stuff out of Model into its own module
import View.Petrinet.Model (Msg, NetElemKind(..), NetInfoWithTypesAndRolesF, PlaceAction(..), Action(..), Tokens, TransitionAction(..), TextBox)
import View.Petrinet.PlaceEditor as PlaceEditor
import View.Petrinet.TransitionEditor as TransitionEditor


-- TODO temp styling niceness hack #88
disableMarkingsAndLabelVisibilityButtons :: Boolean
disableMarkingsAndLabelVisibilityButtons = true

--------------------------------------------------------------------------------

type StateF pid tid ty2 =
  { netInfo                 :: NetInfoWithTypesAndRolesF pid tid Typedef ty2 ()
  , msg                     :: String
  , focusedPlace            :: Maybe pid
  , focusedTransition       :: Maybe tid
  , arcLabelsVisible        :: Boolean
  , placeLabelsVisible      :: Boolean
  , transitionLabelsVisible :: Boolean
  , overrideMarking         :: Maybe (Marking.MarkingF pid Tokens)
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

-- | A Halogen Petri net viewer and editor component.
-- |
-- | The `htmlIdPrefixMaybe` is used to provide distinct HTML element ids, which is necessary to isolate transition
-- | animations to the current component, when you have multiple Petri net editor components on the same HTML page.
-- | Otherwise, firing a transition in one component will also cause firings in other Petri net editor components
-- | to SVG-animate.
-- |
-- | *WARNING*: SVG animations will break when this id contains dashes. Underscores work though.
ui ::
   ∀ pid tid ty2 q m
   . MonadAff m
   => Ord pid
   => Show pid
   => Ord tid
   => Show tid
   => Maybe HtmlId
   -> H.Component HTML q (NetInfoWithTypesAndRolesF pid tid Typedef ty2 ()) Msg m
ui htmlIdPrefixMaybe =
  H.mkComponent { eval: mkEval $ defaultEval { receive = Just <<< LoadNet, handleAction = handleAction }, initialState, render }
  where
    initialState :: NetInfoWithTypesAndRolesF pid tid Typedef ty2 () -> StateF pid tid ty2
    initialState netInfo =
      { netInfo:                 netInfo { netApi = mkNetApiF netInfo.net }
      , msg:                     ""
      , focusedPlace:            empty
      , focusedTransition:       empty
      , placeLabelsVisible:      true
      , transitionLabelsVisible: true
      , arcLabelsVisible:        true
      , overrideMarking:         Nothing
      }

    render :: StateF pid tid ty2 -> ComponentHTML (Action pid tid ty2) () m
    render state =
      div [ classes [ ClassName "flex" ] ]
          [ div [ classes [ ClassName "w-5/6" ] ]
                [ div [ HP.id_ componentHtmlId
                      , classes [ componentClass, ClassName "css-petrinet-component", ClassName $ arcLabelsVisibilityClass <> " " <> transitionLabelsVisibilityClass <> " " <> placeLabelsVisibilityClass ]
                      ]
                      [ SE.svg [ SA.viewBox (_x sceneTopLeft) (_y sceneTopLeft) (_x sceneSize) (_y sceneSize) ]
                               (netToSVG netInfo layout state.focusedPlace state.focusedTransition)
                      -- , HH.br []
                      -- , HH.text state.msg
                      ]
                , div [ classes [ ClassName "w-1/6" ] ] $
                      if disableMarkingsAndLabelVisibilityButtons then [] else
                        [ htmlMarking marking
                        , labelVisibilityButtons
                        ]
                ]
          , div [ classes [ ClassName "w-1/6", ClassName "px-2" ] ]
                [ maybe emptyHtml (mapAction UpdatePlace <<< PlaceEditor.form') do
                    pid <- state.focusedPlace
                    label <- Map.lookup pid state.netInfo.net.placeLabelsDict
                    pure { pid: pid, label: label, typedef: Typedef "Unit", isWriteable: false }
                , maybe emptyHtml (mapAction UpdateTransition <<< TransitionEditor.form' state.netInfo.roleInfos) do
                    tid   <- state.focusedTransition
                    label <- Map.lookup tid state.netInfo.net.transitionLabelsDict
                    typ   <- Map.lookup tid state.netInfo.net.transitionTypesDict
                    let auths = fromMaybe mempty (Map.lookup tid state.netInfo.net.transitionAuthsDict)
                    pure { tid: tid, label: label, typedef: typ, isWriteable: false, auths: auths }
                ]
          ]
      where
        marking = state.overrideMarking # fromMaybe state.netInfo.net.marking
        { layout, textBoxes } = NetInfo.translateAndScale Config.netScale { layout, textBoxes: state.netInfo.textBoxes }
          where
            layout = fromMaybe autoLayout state.netInfo.net.layout
              where
                -- autoLayout = Bipartite.layout Config.bipartiteLayoutScale state.netInfo.net
                autoLayout = Dagre.layout state.netInfo.net

        netInfo = state.netInfo { textBoxes = textBoxes, net { layout = Just layout }}
        sceneSize                       = bounds.max - bounds.min + padding
        sceneTopLeft                    = bounds.min - (padding / pure 2.0)
        bounds                          = NetInfo.boundingBox { layout, textBoxes }
        padding                         = vec2 (4.0 * transitionWidth) (4.0 * transitionHeight)

        arcLabelsVisibilityClass        = guard (not state.arcLabelsVisible)        "css-hide-arc-labels"
        placeLabelsVisibilityClass      = guard (not state.placeLabelsVisible)      "css-hide-place-labels"
        transitionLabelsVisibilityClass = guard (not state.transitionLabelsVisible) "css-hide-transition-labels"

    handleAction :: Action pid tid ty2 -> HalogenM (StateF pid tid ty2) (Action pid tid ty2) () Msg m Unit
    handleAction = case _ of
      LoadNet newNetInfo -> do
        let netInfo = newNetInfo { netApi = mkNetApiF newNetInfo.net }
        H.modify_ (\state -> state { netInfo = netInfo })
      FocusPlace pid -> do
        state <- H.get
        let focusedPlace' = toggleMaybe pid state.focusedPlace
        H.put $ state { focusedPlace = focusedPlace'
                      , msg = (maybe "Focused" (const "Unfocused") state.focusedPlace) <>" place " <> show pid <> " (" <> (fold $ Map.lookup pid state.netInfo.net.placeLabelsDict) <> ")."
                      }
      UpdatePlace (UpdatePlaceLabel pid newLabel) -> do
        H.modify_ $ \state -> state { netInfo = state.netInfo { net = state.netInfo.net {  placeLabelsDict = Map.insert pid newLabel state.netInfo.net.placeLabelsDict } }
                                    , msg = "Updated place " <> show pid <> "."
                                    }
      UpdateTransition (UpdateTransitionName tid newLabel) -> do
        H.modify_ $ \state -> state { netInfo = state.netInfo { net = state.netInfo.net { transitionLabelsDict = Map.insert tid newLabel state.netInfo.net.transitionLabelsDict } }
                                    , msg = "Updated transition " <> show tid <> "."
                                    }
      UpdateTransition (UpdateTransitionType tid newType) -> do
        H.modify_ $ \state -> state { netInfo = state.netInfo { net = state.netInfo.net { transitionTypesDict = Map.insert tid newType state.netInfo.net.transitionTypesDict } }
                                    , msg = "Updated transition " <> show tid <> "."
                                    }
      FocusTransition tid -> do
        state <- H.get
        let focusedTransition' = toggleMaybe tid state.focusedTransition
        H.put $ state { focusedTransition = focusedTransition'
                      , msg = (maybe "Focused" (const "Unfocused") state.focusedTransition) <> " transition " <> show tid <> " (" <> (fold $ Map.lookup tid state.netInfo.net.transitionLabelsDict) <> ")."
                      }
      FireTransition tid -> do
        state <- H.get
        state.netInfo.netApi.transition tid # maybe (pure unit) \t -> do
          let marking' = preMarking t <> state.netInfo.net.marking
          let net' = fire state.netInfo.net t
          H.put $ state { netInfo = state.netInfo { net = net' }
                        , overrideMarking = Just marking'
                        , msg = "Firing transition " <> show tid
                        }
          preCount <- H.liftAff $ SvgUtil.beginElements ("#" <> componentHtmlId <> " ." <> arcAnimationClass tid false)
          H.liftAff $ guard (preCount > 0) $ delay (Milliseconds $ arcAnimationDurationSec * 1000.0)
          postCount <- H.liftAff $ SvgUtil.beginElements ("#" <> componentHtmlId <> " ." <> arcAnimationClass tid true)
          H.liftAff $ guard (postCount > 0) $ delay (Milliseconds $ arcAnimationDurationSec * 1000.0)
          state' <- H.get
          H.put $ state' { overrideMarking = Nothing
                         , msg = "Fired transition " <> show tid <> " (" <> (fold $ Map.lookup tid net'.transitionLabelsDict) <> ")."
                         }
      ToggleLabelVisibility obj -> do
        state <- H.get
        H.put $ case obj of
          Arc ->        state { arcLabelsVisible        = not state.arcLabelsVisible }
          Place ->      state { placeLabelsVisible      = not state.placeLabelsVisible }
          Transition -> state { transitionLabelsVisible = not state.transitionLabelsVisible }

    netToSVG :: NetInfoWithTypesAndRolesF pid tid Typedef ty2 () -> NetLayoutF pid tid -> Maybe pid -> Maybe tid -> Array (ComponentHTML (Action pid tid ty2) () m)
    netToSVG netInfo@{net, netApi} layout focusedPlace focusedTransition =
      svgDefs <> svgTextBoxes <> svgTransitions <> svgPlaces
      where
        svgDefs        = [ SE.defs [] [ svgArrowheadMarker ] ]
        svgTransitions = catMaybes $ map (map svgTransitionAndArcs <<< uncurry mkTransitionAndArcsModel) $ Map.toUnfoldable $ net.transitionsDict
        svgPlaces      = catMaybes $ (map svgPlace <<< mkPlaceModel) <$> net.places
        svgTextBoxes   = svgTextBox <$> netInfo.textBoxes

        mkPlaceModel :: pid -> Maybe (PlaceModelF pid Tokens String Vec2D)
        mkPlaceModel id = do
          label <- Map.lookup id net.placeLabelsDict
          point <- Map.lookup id layout.placePointsDict
          let tokens = Marking.findTokens net.marking id
          pure $ { id: id, tokens: tokens, label: label, point: point, isFocused: id `elem` focusedPlace }

        -- TODO the do-block will fail as a whole if e.g. one findPlacePoint misses
        mkTransitionAndArcsModel :: tid -> TransitionF pid Tokens -> Maybe (TransitionModelF tid String Vec2D)
        mkTransitionAndArcsModel tid tr = do
          trPoint  <- Map.lookup tid layout.transitionPointsDict
          preArcs  <- mkPreArc  trPoint `traverse` tr.pre
          postArcs <- mkPostArc trPoint `traverse` tr.post
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
            mkPostArc :: Vec2D -> PlaceMarkingF pid Tokens -> Maybe (ArcModel tid)
            mkPostArc src tp = { isPost: true, tid: tid, src: src, dest: _, label: arcLabel tp.tokens, htmlId: postArcId tid tp.place } <$> Map.lookup tp.place layout.placePointsDict

            mkPreArc :: Vec2D -> PlaceMarkingF pid Tokens -> Maybe (ArcModel tid)
            mkPreArc dest tp = { isPost: false, tid: tid, src: _, dest: dest, label: arcLabel tp.tokens, htmlId: preArcId tid tp.place } <$> Map.lookup tp.place layout.placePointsDict

            arcLabel :: Tokens -> String
            arcLabel 1 = ""
            arcLabel ts = show ts

    --------------------------------------------------------------------------------

    svgTransitionAndArcs :: TransitionModelF tid String Vec2D -> ComponentHTML (Action pid tid ty2) () m
    svgTransitionAndArcs t =
      SE.g [ SA.class_ $ "css-transition" <> (guard t.isEnabled " enabled") <> " " <> intercalate " " roleClasses
           , SA.id t.htmlId
          --  , HE.onClick (\_ -> Just $ FocusTransition t.id)
           , HE.onClick (\_ -> if t.isEnabled then Just $ FireTransition t.id else Nothing)
           ]
           ((svgArc <$> (t.preArcs <> t.postArcs)) <> [svgTransitionRect t] <> [svgTransitionLabel t])
           where
             roleClasses :: Array String
             roleClasses = map (\r -> "css-role-" <> show r) <<< Set.toUnfoldable <<< un Roles $ t.auths

    svgTransitionRect :: TransitionModelF tid String Vec2D -> ComponentHTML (Action pid tid ty2) () m
    svgTransitionRect t =
      SE.rect [ SA.class_  ("css-transition-rect" <> guard t.isFocused " focused")
              , SA.width   transitionWidth
              , SA.height  transitionHeight
              , SA.x       (_x t.point - transitionWidth / 2.0)
              , SA.y       (_y t.point - transitionHeight / 2.0)
              ]

    svgTransitionLabel :: TransitionModelF tid String Vec2D -> ComponentHTML (Action pid tid ty2) () m
    svgTransitionLabel t =
      SE.text [ SA.class_    "css-transition-name-label"
              , SA.x         (_x t.point)
              , SA.y         (_y t.point - 0.5 * transitionHeight - 7.0 * fontSize)
              , SA.font_size (SA.FontSizeLength $ Em fontSize)
              ]
              [ HH.text t.label ]

    svgArc :: ArcModel tid -> ComponentHTML (Action pid tid ty2) () m
    svgArc arc =
      SE.g [ SA.class_ "css-arc-container" ]
           [ SE.path
               [ SA.class_ $ "css-arc " <> if arc.isPost then "css-post-arc" else "css-pre-arc"
               , SA.id arc.htmlId -- we refer to this as the path of our animation and label, among others
               , SA.d (svgPath arc.src arc.dest)
               ]
           , svgArrow arc.src arc.dest arc.isPost
           , SE.text
              [ SA.class_ "css-arc-name-label"
              , SA.attr (AttrName "dy") "-0.3em"
              , SA.font_size (FontSizeLength $ Em fontSize)
              ]
              [ SE.element (ElemName "textPath") 
                [ SA.attr (AttrName "href") ("#" <> arc.htmlId)
                , SA.attr (AttrName "startOffset") "50%"
                ] [ HH.text arc.label ] 
              ]
           , svgTokenAnimated arc
           ]

    -- | A token that moves along the path of the enclosing arc. This should happen
    -- | when the transition to which this arc is connected fires.
    svgTokenAnimated :: ArcModel tid -> ComponentHTML (Action pid tid ty2) () m
    svgTokenAnimated arc =
      SE.circleNode
        [ SA.class_ "css-token-animated"
        , SA.cx      0.0
        , SA.cy      0.0
        , SA.r       0.0
        ]
        [ SE.animateMotion
            [ SA.id          animationId
            , SA.class_      (arcAnimationClass arc.tid arc.isPost) -- used to trigger the animation with beginElement
            , SA.begin       "indefinite"
            , SA.dur         arcAnimationDuration
            , SA.fillAnim    Freeze
            ]
            [
              SE.mpath [ SA.attr (AttrName "href") $ "#" <> arc.htmlId ] -- token will move along this referenced path
            ]
        , SE.animate
            [ SA.attributeName "opacity"
            , SA.from          "0"
            , SA.to            "1"
            , SA.begin         "indefinite" -- begin="animationId.begin" doesn't seem to work in Chrome when DOM is created dynamically
            , SA.dur           tokenFadeDuration
            , SA.fillAnim      Freeze
            , SA.class_        (arcAnimationClass arc.tid arc.isPost) -- used to trigger the animation with beginElement
            ]
        , SE.animate
            [ SA.attributeName "r"
            , SA.from          (show 0.0)
            , SA.to            (show $ 2.0 * tokenRadius)
            , SA.begin         "indefinite"
            , SA.dur           tokenFadeDuration
            , SA.fillAnim      Freeze
            , SA.class_        (arcAnimationClass arc.tid arc.isPost) -- used to trigger the animation with beginElement
            ]
        ]
      where
        animationId = tokenAnimatedClass arc.htmlId
        tokenFadeDuration = arcAnimationDuration <#> (_ / 4.0)

    --------------------------------------------------------------------------------

    svgPlace :: PlaceModelF pid Tokens String Vec2D -> ComponentHTML (Action pid tid ty2) () m
    svgPlace { id: id, label: label, point: point, tokens: tokens, isFocused: isFocused } =
      SE.g [ SA.id (mkPlaceIdStr id)
          --  , HE.onClick (\_ -> Just $ FocusPlace id)
           ]
           [ SE.title [] [ Core.text label ]
           , SE.circle
               [ SA.class_ ("css-place" <> guard isFocused " focused")
               , SA.r      placeRadius
               , SA.cx     (_x point)
               , SA.cy     (_y point)
               ]
           , svgTokens
           , SE.text [ SA.class_    "css-place-name-label"
                     , SA.x         (_x point)
                     , SA.y         (_y point + placeRadius + 16.0 * fontSize)
                     , SA.font_size (SA.FontSizeLength $ Em fontSize)
                     ]
                     [ HH.text label ]
           , SE.text [ SA.class_    "css-place-label"
                     , SA.x         (_x point + tokenPadding)
                     , SA.y         (_y point - tokenPadding)
                     , SA.font_size (SA.FontSizeLength $ Em fontSize)
                     ]
                     [ HH.text $ if tokens < 6 then "" else show tokens ]
           ]
      where
        svgTokens :: ComponentHTML (Action pid tid ty2) () m
        svgTokens = if tokens == 0 then HH.text "" else
          if (tokens == 1 || tokens > 5) then
            SE.circle
              [ SA.r      tokenRadius
              , SA.cx     (_x point)
              , SA.cy     (_y point)
              , SA.class_ "css-token-in-place"
              ]
          else
            SE.g [] $ (1..tokens) <#> \i -> 
              SE.circle
              [ SA.r      tokenRadius
              , SA.cx     (_x point + cos (toNumber i / toNumber tokens * 2.0 * pi) * placeRadius * r tokens)
              , SA.cy     (_y point + sin (toNumber i / toNumber tokens * 2.0 * pi) * placeRadius * r tokens)
              , SA.class_ "css-token-in-place"
              ]
              where
                r 2 = 0.33
                r 3 = 0.4
                r _ = 0.5

    --------------------------------------------------------------------------------

    svgTextBox :: TextBox -> ComponentHTML (Action pid tid ty2) () m
    svgTextBox tb =
      SE.g [ SA.class_ "css-textbox" ]
           [ SE.rect [ SA.x       x
                     , SA.y       y
                     , SA.width   w
                     , SA.height  h
                     ]
           , SE.text [ SA.class_    "css-textbox-label"
                     , SA.x         (x + 15.0 * fontSize) -- TODO offset computation is wrong (a temp hack) #88
                     , SA.y         (y + 20.0 * fontSize) -- TODO offset computation is wrong (a temp hack) #88
                     , SA.font_size (SA.FontSizeLength $ Em fontSize)
                     ]
                     [ HH.text tb.text ]
           ]
      where
        { topLeft, bottomRight } = unwrap tb.box
        boxSize                  = bottomRight - topLeft
        x                        = _x topLeft
        y                        = _y topLeft
        w                        = _x boxSize
        h                        = _y boxSize

    --------------------------------------------------------------------------------

    componentClass :: ClassName
    componentClass = ClassName "petrinet-component"

    componentHtmlId :: HtmlId
    componentHtmlId = netPrefix <> un ClassName componentClass

    -- | Provide distinct id's for distinct nets in a webpage.
    netPrefix :: String
    netPrefix = foldMap (_ <> "_") htmlIdPrefixMaybe

    prefixTransition :: tid -> HtmlId
    prefixTransition tid = "t" <> show tid

    prefixPlace :: pid -> HtmlId
    prefixPlace pid = "p" <> show pid

    postArcId :: tid -> pid -> HtmlId
    postArcId tid place = netPrefix <> "arc_" <> prefixTransition tid <> "_" <> prefixPlace place

    preArcId :: tid -> pid -> HtmlId
    preArcId tid place = netPrefix <> "arc_" <> prefixPlace place <> "_" <> prefixTransition tid

    arcAnimationClass :: tid -> Boolean -> HtmlId
    arcAnimationClass tid isPost = prefix isPost <> prefixTransition tid # append netPrefix # tokenAnimatedClass
      where
        prefix true = "arc_post_"
        prefix false = "arc_pre_"

    tokenAnimatedClass x = x <> "_token_animated"

    mkTransitionIdStr :: tid -> HtmlId
    mkTransitionIdStr = append netPrefix <<< prefixTransition

    mkPlaceIdStr :: pid -> HtmlId
    mkPlaceIdStr = append netPrefix <<< prefixPlace

--------------------------------------------------------------------------------

htmlMarking :: ∀ a n pid tid ty2 m. Show a => Show n => BagF a n -> ComponentHTML (Action pid tid ty2) () m
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
    rows = map (uncurry tr) <<< Marking.toUnfoldable $ bag
    tr k v = HH.tr [] [ HH.td [] [ HH.text $ show k ]
                      , HH.td [] [ HH.text $ show v ]
                      ]

--------------------------------------------------------------------------------

labelVisibilityButtons :: ∀ pid tid ty2 m. ComponentHTML (Action pid tid ty2) () m
labelVisibilityButtons =
  div [ classes [ ClassName "field has-addons" ] ]
      [ HH.p [ classes [ ClassName "control" ] ]
             [ HH.a [ classes [ ClassName "button", ClassName "is-small" ]
                    , HE.onClick $ \_ -> Just $ ToggleLabelVisibility Place ]
                    [ HH.span [] [ HH.text "Place labels" ] ]
             ]
      , HH.p [ classes [ ClassName "control" ] ]
             [ HH.a [ classes [ ClassName "button", ClassName "is-small" ]
                    , HE.onClick $ \_ -> Just $ ToggleLabelVisibility Transition ]
                    [ HH.span [] [ HH.text "Transition labels" ] ]
             ]
      , HH.p [ classes [ ClassName "control" ] ]
             [ HH.a [ classes [ ClassName "button", ClassName "is-small" ]
                    , HE.onClick $ \_ -> Just $ ToggleLabelVisibility Arc ]
                    [ HH.span [] [ HH.text "Arc labels" ] ]
             ]
      ]

--------------------------------------------------------------------------------

svgPath :: Vec2D -> Vec2D -> Array SA.D
svgPath p q = SA.Abs <$> [ SA.M (_x p) (_y p), SA.L (_x q) (_y q) ]

toggleMaybe :: ∀ a b. b -> Maybe a -> Maybe b
toggleMaybe z mx = case mx of
  Nothing -> Just z
  Just _  -> Nothing
