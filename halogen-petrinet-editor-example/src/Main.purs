module Main where

import Prelude
import Data.Array (findIndex, length, (..), zip)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Map as Map
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Halogen.Aff (awaitLoad, runHalogenAff)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)
import Record (union)
import Web.DOM.ParentNode (QuerySelector(..))

import ExampleData as Ex
import View.Petrinet.PetrinetEditor as PetrinetEditor
import View.Petrinet.Model (mkNetRepUsingLayout, mkNetApi)
import Data.Petrinet.Representation.Marking as Marking
import Data.Vec3 (vec2)


main :: Effect Unit
main = runHalogenAff do
  awaitLoad

  let net1 = Ex.netInfo1 `union` { roleInfos: [], types: [] }
  net1ElemMaybe <- selectElement (QuerySelector "#net1-container")
  _ <- runUI (PetrinetEditor.ui (Just "net1")) net1 `traverse` net1ElemMaybe

  let net2 = Ex.netInfo2 `union` { roleInfos: [], types: [] }
  net2ElemMaybe <- selectElement (QuerySelector "#net2-container")
  runUI (PetrinetEditor.ui (Just "net2")) net2 `traverse` net2ElemMaybe

renderNet :: String -> JSNetInfo -> Aff Unit
renderNet sel jsNetInfo = do
  let toPlaceId name = findIndex (\p -> p.label == name) jsNetInfo.net.places # maybe 0 (_ + 1) 
  let toPlaceIds = map (\(k /\ v) -> toPlaceId k /\ v) :: forall a. Array (String /\ a) -> Array (Int /\ a)
  let numPlaces = length jsNetInfo.net.places
  let places = 1 .. numPlaces
  let placeLabels = places `zip` (jsNetInfo.net.places <#> _.label)
  let toPrePost = Object.toUnfoldable >>> toPlaceIds >>> map \(k /\ v) -> { place: k, tokens: v }
  let transitions = jsNetInfo.net.transitions <#> \t -> { pre: toPrePost t.pre, post: toPrePost t.post }
  let transitionLabels = jsNetInfo.net.transitions <#> _.label
  let marking = Marking.fromFoldable (toPlaceIds $ Object.toUnfoldable jsNetInfo.net.marking :: Array (Int /\ Int))
  let layout = { 
    placePointsDict: jsNetInfo.net.places <#> (\p -> p.label /\ vec2 p.x p.y) # toPlaceIds # Map.fromFoldable,
    transitionPointsDict: jsNetInfo.net.transitions <#> (\t -> vec2 t.x t.y) # zip ((numPlaces + 1)..(numPlaces + 20)) # Map.fromFoldable }
  let netRep = mkNetRepUsingLayout places transitions marking placeLabels transitionLabels (Just layout) mempty mempty
  let netInfo = { name: jsNetInfo.title, net: netRep, netApi: mkNetApi netRep, textBoxes: mempty }
  netElemMaybe <- selectElement (QuerySelector sel)
  _ <- runUI (PetrinetEditor.ui (Just jsNetInfo.title)) (netInfo `union` { roleInfos: [], types: [] }) `traverse` netElemMaybe
  pure unit
  

type JSNetInfo = 
  { title :: String
  , net :: 
    { places :: Array 
      { label :: String
      , x :: Number
      , y :: Number
      }
    , transitions :: Array
      { label :: String
      , x :: Number
      , y :: Number
      , pre :: Object.Object Int
      , post :: Object.Object Int
      }
    , marking :: Object.Object Int
    } 
  }