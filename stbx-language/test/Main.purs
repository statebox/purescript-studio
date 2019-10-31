module Test.Main where

import Prelude
import Data.Bifunctor (bimap)
import Data.List as List
import Data.List (List(..))
import Data.String (trim)
import Data.Tuple (fst)
import Effect.Aff (Aff, launchAff)
import Effect (Effect)
import Effect.Class.Console (log)
import Language.Statebox as Statebox
import Language.Statebox.Hypergraph (NodeF(..), HyperEdgeF(..), GElemF(..))
import Language.Statebox.Net.AST (Node(..), GElem(..), HyperEdge(..), Label, Span, LabelWithSpan, LabelWithSpanWithType, getLabel, nodeLabel, nodeLabelWithSpan)
import Language.Statebox.Net.Generator (mkParseResult) as Generator
import Language.Statebox.Net.Generator.Net as Net
import Test.Spec                  (describe, pending, it)
import Test.Spec.Console          (write)
import Test.Spec.Runner           (runSpec)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import Debug.Trace (spy)


net1src :: String
net1src = trim """
login    : guest -> loggedIn
buy      : loggedIn               -> loggedIn, cartNonEmpty
checkout : loggedIn, cartNonEmpty -> loggedOut
"""

net1expected :: List (GElemF List String String)
net1expected = mkAst
  [ mkEdge "login"    ["guest"]                    ["loggedIn"]
  , mkEdge "buy"      ["loggedIn"]                 ["loggedIn", "cartNonEmpty"]
  , mkEdge "checkout" ["loggedIn", "cartNonEmpty"] ["loggedOut"]
  ]

main :: Effect _
main = launchAff $ runSpec [consoleReporter] do
  describe "petri net parser" do
    it "should do stuff" do
      let ast = Statebox.parseNet net1src
      let astDump = spy "ast" $ show $ ast
      let pr1 = spy "pr1" $ Generator.mkParseResult <$> ast
      let net1 = spy "net1" $ Net.toNetWithDefaultName "dummy" <$> ast
      (ast # map (map (bimap dropAnnotations dropAnnotations))) `shouldEqual` pure net1expected


--------------------------------------------------------------------------------
-- graph DSL
--------------------------------------------------------------------------------

-- | Drop the type and span, leaving only the label.
dropAnnotations = fst <<< fst

mkAst gelems = List.fromFoldable gelems

mkEdge el sls tls = GHyperEdge (HyperEdge el (List.fromFoldable $ map Node sls)
                                             (List.fromFoldable $ map Node tls))
