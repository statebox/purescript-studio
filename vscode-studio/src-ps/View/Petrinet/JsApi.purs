module View.Petrinet.JsApi where

import Prelude hiding (div)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import View.Common (HtmlId)
import Halogen.Aff (awaitLoad, runHalogenAff)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)
import Effect (Effect)
import Effect.Console (log)
import Record (union)
import Web.DOM.ParentNode (QuerySelector(..))

import View.Petrinet.Model (NetInfoF)
import View.Petrinet.Model.NLL (toNetInfoWithDefaults) as NLL
import View.Petrinet.PetrinetEditor (ui) as PetrinetEditor
import Data.Petrinet.Representation.NLL (fromNLLMaybe) as Net
import Data.Typedef (Typedef)
import Statebox.Core.Types (Net)

-- | A convenience method for putting the PetrinetEditor component on a page.
-- | Example: `runPetrinetEditorUI "#my_net_container" "my_net" myNetInfo`.
runPetrinetEditorUI' ::
   ∀ pid tid
   . Ord pid => Ord tid => Show pid => Show tid
  => String -> Maybe HtmlId -> NetInfoF pid tid Typedef () -> Effect Unit
runPetrinetEditorUI' querySelector htmlIdPrefixMaybe netInfo =
  runHalogenAff do
    awaitLoad
    let netInfo' = netInfo `union` { roleInfos: [], types: [] }
    htmlElemMaybe <- selectElement (QuerySelector querySelector)
    runUI (PetrinetEditor.ui htmlIdPrefixMaybe) netInfo' `traverse` htmlElemMaybe

runPetrinetEditorUI_NLL :: String -> HtmlId -> Net -> Effect Unit
runPetrinetEditorUI_NLL querySelector htmlIdPrefix net =
  runPetrinetEditorUI' querySelector (Just htmlIdPrefix) netInfo
  where
    -- TODO Look at this function and observe how close it is to just taking a Net and converting that to NetInfo;
    --      there's some refactoring to be done there.
    -- TODO Parse names.
    netInfo = NLL.toNetInfoWithDefaults partition net.name placeNames net.names
    placeNames = fromMaybe ["ERROR no place names found in Net record"] net.placeNames
    partitionM = Net.fromNLLMaybe 0 net.partition
    -- TODO -1
    partition = fromMaybe [[-1] /\ [-1]] partitionM


main :: Effect Unit
main = log "PetrinetEditor: JsApi.main: looks like I've been loaded!"
