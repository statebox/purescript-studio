module View.Petrinet.PetrinetEditor where

import Prelude hiding (div)
import Control.MonadZero (empty)
import Data.Array (catMaybes)
import Data.Newtype (un, unwrap)
import Data.Bag (BagF)
import Data.Foldable (fold, foldMap, elem, intercalate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map as Map
import Data.Monoid (guard)
import Data.Monoid.Additive (Additive(..))
import Data.Set as Set
import Data.Tuple (uncurry)
import Data.Traversable (traverse)
import Data.Vec3 (Vec2D, vec2, _x, _y)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen (ComponentHTML, HalogenM, mkEval, defaultEval)
import Halogen.HTML as HH
import Halogen.HTML (HTML, div)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Core (ClassName(..), ElemName(..), AttrName(..))
import Halogen.HTML.Core as Core
import Halogen.HTML.Events as HE
import Svg.Elements as SE
import Svg.Attributes as SA
import Svg.Attributes (CSSLength(..), FillState(..), FontSize(..), seconds)
import Svg.Util as SvgUtil

import Data.Auth (Roles(..))
import Data.Petrinet.Representation.Dict (TransitionF, NetLayoutF, PlaceMarkingF, isTransitionEnabled, fire, mkNetApiF)
import Data.Petrinet.Representation.Layout.Bipartite as Layout.Bipartite
import Data.Petrinet.Representation.Marking as Marking
import Data.Typedef (Typedef(..))

import View.Common (HtmlId, emptyHtml, mapAction)
import View.Petrinet.Arrow (svgArrow, svgArrowheadMarker)
import View.Petrinet.Config as Config
import View.Petrinet.Config (placeRadius, transitionWidth, transitionHeight, tokenRadius, tokenPadding, fontSize, arcAnimationDuration)
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
      { netInfo:                 scaledNetInfo
      , msg:                     ""
      , focusedPlace:            empty
      , focusedTransition:       empty
      , placeLabelsVisible:      true
      , transitionLabelsVisible: true
      , arcLabelsVisible:        true
      }
      where
-- TODO hmm, dit lijkt NIET de anim init bug te fixen, maar WEL de layout init bug
        scaledNetInfo = NetInfo.translateAndScale Config.netScale netInfo { netApi = mkNetApiF netInfo.net }

    render :: StateF pid tid ty2 -> ComponentHTML (Action pid tid ty2) () m
    render state =
      div [ classes [ ClassName "flex" ] ]
          [ div [ classes [ ClassName "w-5/6" ] ]
                [ div [ HP.id_ componentHtmlId
                      , classes [ componentClass, ClassName "css-petrinet-component", ClassName $ arcLabelsVisibilityClass <> " " <> transitionLabelsVisibilityClass <> " " <> placeLabelsVisibilityClass ]
                      ]
                      [ SE.svg [ SA.viewBox (_x sceneTopLeft) (_y sceneTopLeft) (_x sceneSize) (_y sceneSize) ]
                               (netToSVG state.netInfo layout state.focusedPlace state.focusedTransition)
                      , HH.br []
                      , HH.text state.msg
                      ]
                , div [ classes [ ClassName "w-1/6" ] ] $
                      if disableMarkingsAndLabelVisibilityButtons then [] else
                        [ htmlMarking state.netInfo.net.marking
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
-- TODO - we don't take the 'scale' into account b/c the viewport doesn't scale along, so any other tweaking is useless
-- TODO - komt die animatie-bug door een init error in de berekening van de scene size op basis van bijv de verkeerde bounds/layout/net/whatevs? want het gebeurt dus nu ook weer als we elk net layouten
-- TODO - gaat dat mis in de afhandeling van LoadNet?
        sceneSize                       = bounds.max - bounds.min + padding
        sceneTopLeft                    = bounds.min - (padding / pure 2.0)
        bounds                          = NetInfo.boundingBox state.netInfo
        padding                         = vec2 (4.0 * transitionWidth) (4.0 * transitionHeight)

        arcLabelsVisibilityClass        = guard (not state.arcLabelsVisible)        "css-hide-arc-labels"
        placeLabelsVisibilityClass      = guard (not state.placeLabelsVisible)      "css-hide-place-labels"
        transitionLabelsVisibilityClass = guard (not state.transitionLabelsVisible) "css-hide-transition-labels"

        layout :: NetLayoutF pid tid
        layout = fromMaybe autoLayout state.netInfo.net.layout
          where
            autoLayout = Layout.Bipartite.bipartite Config.bipartiteLayoutScale state.netInfo.net

    handleAction :: ∀ tid. Ord tid => Show tid => Action pid tid ty2 -> HalogenM (StateF pid tid ty2) (Action pid tid ty2) () Msg m Unit
    handleAction = case _ of
      LoadNet newNetInfo -> do
        let scaledNetInfo = NetInfo.translateAndScale Config.netScale newNetInfo { netApi = mkNetApiF newNetInfo.net }
        H.modify_ (\state -> state { netInfo = scaledNetInfo })
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
        numElems <- H.liftAff $ SvgUtil.beginElements ("#" <> componentHtmlId <> " ." <> arcAnimationClass tid)
        state <- H.get
        let
          netMaybe' = fire state.netInfo.net <$> state.netInfo.netApi.transition tid
          net'      = fromMaybe state.netInfo.net netMaybe'
        H.put $ state { netInfo = state.netInfo { net = net'}
                      , msg = "Fired transition " <> show tid <> " (" <> (fold $ Map.lookup tid net'.transitionLabelsDict) <> ")."
                      }
      ToggleLabelVisibility obj -> do
        state <- H.get
        H.put $ case obj of
          Arc ->        state { arcLabelsVisible        = not state.arcLabelsVisible }
          Place ->      state { placeLabelsVisible      = not state.placeLabelsVisible }
          Transition -> state { transitionLabelsVisible = not state.transitionLabelsVisible }

    netToSVG :: ∀ tid a. Ord pid => Show pid => Ord tid => Show tid => NetInfoWithTypesAndRolesF pid tid Typedef ty2 () -> NetLayoutF pid tid -> Maybe pid -> Maybe tid -> Array (ComponentHTML (Action pid tid ty2) () m)
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
            mkPostArc tid src tp = { isPost: true, tid: tid, src: src, dest: _, label: arcLabel tp.tokens, htmlId: postArcId tid tp.place } <$> Map.lookup tp.place layout.placePointsDict

            mkPreArc :: ∀ tid a. Show tid => tid -> Vec2D -> PlaceMarkingF pid Tokens -> Maybe (ArcModel tid)
            mkPreArc tid dest tp = { isPost: false, tid: tid, src: _, dest: dest, label: arcLabel tp.tokens, htmlId: preArcId tid tp.place } <$> Map.lookup tp.place layout.placePointsDict

            arcLabel :: Tokens -> String
            arcLabel 1 = ""
            arcLabel ts = show ts

    --------------------------------------------------------------------------------

    svgTransitionAndArcs :: ∀ tid m. Show tid => TransitionModelF tid String Vec2D -> ComponentHTML (Action pid tid ty2) () m
    svgTransitionAndArcs t =
      SE.g [ SA.class_ $ "css-transition" <> (guard t.isEnabled " enabled") <> " " <> intercalate " " roleClasses
           , SA.id t.htmlId
           , HE.onClick (\_ -> Just $ FocusTransition t.id)
           , HE.onDoubleClick (\_ -> Just $ if t.isEnabled then FireTransition t.id else FocusTransition t.id)
           ]
           ((svgArc <$> (t.preArcs <> t.postArcs)) <> [svgTransitionRect t] <> [svgTransitionLabel t])
           where
             roleClasses :: Array String
             roleClasses = map (\r -> "css-role-" <> show r) <<< Set.toUnfoldable <<< un Roles $ t.auths

    svgTransitionRect :: ∀ tid m. Show tid => TransitionModelF tid String Vec2D -> ComponentHTML (Action pid tid ty2) () m
    svgTransitionRect t =
      SE.rect [ SA.class_  ("css-transition-rect" <> guard t.isFocused " focused")
              , SA.width   transitionWidth
              , SA.height  transitionHeight
              , SA.x       (_x t.point - transitionWidth / 2.0)
              , SA.y       (_y t.point - transitionHeight / 2.0)
              ]

    svgTransitionLabel :: ∀ tid m. Show tid => TransitionModelF tid String Vec2D -> ComponentHTML (Action pid tid ty2) () m
    svgTransitionLabel t =
      SE.text [ SA.class_    "css-transition-name-label"
              , SA.x         (_x t.point)
              , SA.y         (_y t.point - 0.65 * transitionHeight + 0.25 * fontSize)
              , SA.font_size (SA.FontSizeLength $ Em fontSize)
              ]
              [ HH.text t.label ]

    svgArc :: ∀ pid tid m. Show tid => ArcModel tid -> ComponentHTML (Action pid tid ty2) () m
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
    svgTokenAnimated :: ∀ pid tid m. Show tid => ArcModel tid -> ComponentHTML (Action pid tid ty2) () m
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
        , SE.animate -- hide the token after the animation completes
            [ SA.attributeName "r"
            , SA.to            (show 0.0)
            , SA.begin         (animationId <> ".end")
            , SA.dur           (seconds 0.0001) -- we want this immediately after the animation ends
            , SA.fillAnim      Freeze
            , SA.repeatCount   0
            ]
        ]
      where
        animationId = tokenAnimatedClass arc.htmlId
        tokenFadeDuration = arcAnimationDuration <#> (_ / 4.0)

    --------------------------------------------------------------------------------

    svgPlace :: ∀ pid tid m. Show pid => PlaceModelF pid Tokens String Vec2D -> ComponentHTML (Action pid tid ty2) () m
    svgPlace { id: id, label: label, point: point, tokens: tokens, isFocused: isFocused } =
      SE.g [ SA.id (mkPlaceIdStr id)
           , HE.onClick (\_ -> Just $ FocusPlace id)
           ]
           [ SE.title [] [ Core.text label ]
           , SE.circle
               [ SA.class_ ("css-place" <> guard isFocused " focused")
               , SA.r      placeRadius
               , SA.cx     (_x point)
               , SA.cy     (_y point)
               ]
           , svgTokens tokens point
           , SE.text [ SA.class_    "css-place-name-label"
                     , SA.x         (_x point)
                     , SA.y         (_y point + 2.0 * placeRadius + 0.25 * fontSize)
                     , SA.font_size (SA.FontSizeLength $ Em fontSize)
                     ]
                     [ HH.text label ]
           , SE.text [ SA.class_    "css-place-label"
                     , SA.x         (_x point + tokenPadding)
                     , SA.y         (_y point - tokenPadding)
                     , SA.font_size (SA.FontSizeLength $ Em fontSize)
                     ]
                     [ HH.text $ if tokens == 0 || tokens == 1 then "" else show tokens ]
           ]
      where
        svgTokens :: Tokens -> Vec2D -> ComponentHTML (Action pid tid ty2) () m
        svgTokens tokens point = if Additive tokens == mempty then HH.text "" else
          SE.circle
            [ SA.r      tokenRadius
            , SA.cx     (_x point)
            , SA.cy     (_y point)
            , SA.class_ "css-token-in-place"
            ]

    --------------------------------------------------------------------------------

    svgTextBox :: ∀ tid m. TextBox -> ComponentHTML (Action pid tid ty2) () m
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
