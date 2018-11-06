module PetrinetView where

import Prelude
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Data.Monoid.Additive (Additive(..))
import Data.Tuple (Tuple(..), uncurry, snd)
import Data.Traversable (traverse, sequence)
import Data.Vec2D (Vec2D)
import Data.Vec2D (bounds) as Vec2D
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML (div)
import Halogen.HTML (HTML)
import Halogen.HTML.Events as HE
import Svg.Elements as SE
import Svg.Attributes as SA
import Math (atan2, cos, sin, sqrt)

import ExampleData as Ex
import ExampleData as Net
import Data.Petrinet.Representation.Dict
import ExampleData (PID, TID, Tokens, NetRep, NetApiF)
import Svg.Arrow

data Query tid a = ClickTransition tid a

type State tid =
  { net    :: NetRepF PID tid Tokens
  , netApi :: NetApiF PID tid Tokens
  , msg    :: String
  }

ui :: ∀ tid g. Show tid => State tid -> H.Component HTML (Query tid) Unit Void g
ui initialState = H.component { initialState: const initialState, render, eval, receiver: const Nothing }
  where
  render :: State tid -> H.ComponentHTML (Query tid)
  render state =
    div []
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
    -- TODO maybe stick the padding inside the bounding box?
    paddingX = 4.0 * transitionWidth
    paddingY = 4.0 * transitionHeight

  eval :: (Query tid) ~> H.ComponentDSL (State tid) (Query tid) Void g
  eval (ClickTransition transitionId next) = do
    H.modify_ mod1
    pure next
    where
      mod1 state =
        state { net = net'
              , msg = "transition " <> show transitionId <> " clicked!"
                   <> "\n\n"
                   <> show (_.marking <$> netMaybe')
              }
        where
          netMaybe' = fire state.net <$> state.net.findTransition transitionId
          net'      = fromMaybe state.net $ netMaybe'

--------------------------------------------------------------------------------

type PlaceModelF pid tok label pt =
  { id     :: pid
  , tokens :: tok
  , label  :: label
  , point  :: pt
  }

netToSVG :: ∀ pid tid a. Ord pid => NetRepF pid tid Tokens -> Array (HTML a ((Query tid) Unit))
netToSVG net =
  svgPlaces <> svgTransitions <> svgDefs
  where
    svgDefs = pure (SE.g [] [ arrowHead ])

    svgTransitions = join $ fromMaybe [] $ traverse (uncurry drawTransitionAndArrows) $ Map.toUnfoldable $ net.transitionsDict

    svgPlaces = fromMaybe [] $ drawPlace1 `traverse` net.places

    drawPlace1 :: pid -> Maybe (HTML a ((Query tid) Unit))
    drawPlace1 id = do
      label <- net.findPlaceLabel id
      point <- net.findPlacePoint id
      let tokens = findTokens net id
      pure $ svgPlace { id: id, tokens: tokens, label: label, point: point }

    drawTransitionAndArrows :: ∀a. tid -> TransitionF pid Tokens -> Maybe (Array (HTML a ((Query tid) Unit)))
    drawTransitionAndArrows tid tr = do
      trPos <- net.findTransitionPoint tid

      preArrows  <- arrowFrom trPos `traverse` tr.pre
      postArrows <- arrowTo   trPos `traverse` tr.post

      pure $ preArrows <> postArrows <> [ svgTransition trPos tid ]

    -- arrow from transition to place
    arrowTo :: ∀a. Vec2D -> PlaceMarkingF pid Tokens -> Maybe (HTML a ((Query tid) Unit))
    arrowTo dest tp = transToPlace <$> net.findPlacePoint tp.place <*> pure dest

    -- arrow from place to transition
    arrowFrom :: ∀a. Vec2D -> PlaceMarkingF pid Tokens -> Maybe (HTML a ((Query tid) Unit))
    arrowFrom src tp = placeToTrans <$> pure src                   <*> net.findPlacePoint tp.place

--------------------------------------------------------------------------------

-- TODO ported from Elm, but probably still a good idea to not use show directly
toString = show

svgPath :: Vec2D -> Vec2D -> _
svgPath p q = SA.d $ SA.Abs <$> [ SA.M p.x p.y, SA.L q.x q.y ]

placeRadius :: Number
placeRadius = 4.0 * tokenRadius

tokenRadius :: Number
tokenRadius = 1.0

color1 = SA.RGB 250 0 100

tokenColorFill = color1
tokenColorStroke = color1

transitionHeight = 2.0 * placeRadius
transitionWidth  = 2.0 * placeRadius

svgPlace :: ∀ a pid tid. PlaceModelF pid Tokens String Vec2D -> HTML a ((Query tid) Unit)
svgPlace { id: id, label: label, point: point, tokens: tokens } =
  SE.g []
       [
         SE.circle
           [ SA.r      placeRadius
           , SA.cx     point.x
           , SA.cy     point.y
           , SA.fill   $ Just (SA.RGB 250 250 0)
           , SA.stroke $ Just color1
           ]
       , svgTokens tokens point
       , SE.text [ SA.x     point.x
                 , SA.y     (point.y - 1.0 * placeRadius)
                 , SA.fill   $ Just (SA.RGB 200 200 200)
                 -- , SA.stroke $ Just color1
                 -- , SA.font_size SA.XXSmall
                 , SA.font_size (SA.FontSizeLength (SA.Px 4.0))
                 ]
                 -- [ HH.text label ]
                 [ HH.text $ show tokens ]
       ]
  where
    svgTokens :: Tokens -> Vec2D -> HTML a ((Query tid) Unit)
    svgTokens tokens point = if Additive tokens == mempty then HH.text "" else
      SE.circle
        [ SA.r      tokenRadius
        , SA.cx     point.x
        , SA.cy     point.y
        , SA.fill   $ Just tokenColorFill
        , SA.stroke $ Just tokenColorStroke
        ]

svgTransition :: ∀ a tid . Vec2D -> tid -> HTML a ((Query tid) Unit)
svgTransition p tid = SE.rect
  [ SA.width   transitionWidth
  , SA.height  transitionHeight
  , SA.x       (p.x - transitionWidth / 2.0)
  , SA.y       (p.y - transitionHeight / 2.0)
  , SA.fill    (Just (SA.RGB 250 250 0))
  , SA.stroke  (Just (SA.RGB 250 0   100))
  , HE.onClick (HE.input_ (ClickTransition tid))
  ]
