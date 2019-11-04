module Test.Language.Statebox.Net where

import Prelude
import Data.Bifunctor (bimap)
import Data.List as List
import Data.List (List(..))
import Data.String (trim)
import Effect (Effect)
import Effect.Class.Console (log)
import Language.Statebox as Statebox
import Language.Statebox.Hypergraph (NodeF(..), HyperEdgeF(..), GElemF(..))
import Language.Statebox.Net.AST (Node(..), GElem(..), HyperEdge(..), Label, Span, LabelWithSpan, LabelWithSpanWithType, stripTypeAndSpan)
import Language.Statebox.Net.Generator (mkParseResult) as Generator
import Language.Statebox.Net.Generator.Net as Net
import Test.Spec                  (Spec, describe, pending, it)
import Test.Spec.Console          (write)
import Test.Spec.Runner           (runSpec)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import Debug.Trace (spy)

spec :: Spec _
spec = do
  describe "Statebox Petri net compiler" do
    it "should parse a net correctly" do
      let ast = Statebox.parseNet net1src
      let astDump = spy "ast" $ show $ ast
      let pr1 = spy "pr1" $ Generator.mkParseResult <$> ast
      let net1 = spy "net1" $ Net.toNetWithDefaultName "dummy" <$> ast
      (ast # map (map (bimap stripTypeAndSpan stripTypeAndSpan))) `shouldEqual` pure net1expected

net1src :: String
net1src = trim """
login    : guest -> loggedIn
buy      : loggedIn               -> loggedIn, cartNonEmpty
checkout : loggedIn, cartNonEmpty -> loggedOut
"""

net1expected :: List (GElemF List Label Label)
net1expected = mkAst
  [ mkEdge "login"    ["guest"]                    ["loggedIn"]
  , mkEdge "buy"      ["loggedIn"]                 ["loggedIn", "cartNonEmpty"]
  , mkEdge "checkout" ["loggedIn", "cartNonEmpty"] ["loggedOut"]
  ]


--------------------------------------------------------------------------------
-- graph DSL
--------------------------------------------------------------------------------

mkAst gelems = List.fromFoldable gelems

mkEdge e ss ts = GHyperEdge (HyperEdge e (List.fromFoldable ss) (List.fromFoldable ts))
