module Test.Language.Statebox.Wiring where

import Prelude
import Data.Bifunctor (lmap)
import Data.List as List
import Data.List (List)
import Data.String (trim)
import Language.Statebox as Stbx
import Language.Statebox.Hypergraph (HyperEdgeF(..), GElemF(..))
import Language.Statebox.Wiring.AST (Label, stripSpan)
import Language.Statebox.Wiring.Generator.Diagram (toDiagramWithName) as DiagramV1
import Language.Statebox.Wiring.Generator.DiagramV2 as DiagramV2
import Statebox.Core.Types (Diagram)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Debug.Trace (spy)

spec :: Spec Unit
spec = do
  describe "Statebox wiring compiler" do
    let
      diagramSrc :: String
      diagramSrc = trim """
        a1 -> b1, b2
        b1 -> d1
        b2 -> c1
        c1 -> d1
        """

      diagramAstExpected :: List (GElemF List Label Unit)
      diagramAstExpected = mkAst
        [ mkEdge ["a1"] ["b1", "b2"]
        , mkEdge ["b1"] ["d1"]
        , mkEdge ["b2"] ["c1"]
        , mkEdge ["c1"] ["d1"]
        ]

      diagramV1Expected :: Diagram
      diagramV1Expected =
        { name: "diagramV1"
        , width: 5
        , pixels: [ 1, 1, 2, 3, 5, 2, 3, 4, 5, 4 ]
        , names: [ "a1", "b1", "b2", "d1", "c1" ]
        }

    it "should parse a diagram correctly" do
      let
        diagramAstWithSpans = Stbx.parseDiagram diagramSrc
        diagramAst = diagramAstWithSpans # map (map (lmap stripSpan))
      diagramAst `shouldEqual` pure diagramAstExpected

    it "should compile a diagram AST to the corresponding Diagram (v1)" do
      let
        diagramAstWithSpans = Stbx.parseDiagram diagramSrc -- TODO we don't want spans here
        diagramV1 = DiagramV1.toDiagramWithName "diagramV1" <$> diagramAstWithSpans
      diagramV1 `shouldEqual` pure diagramV1Expected

    it "should convert a Diagram (v1) to its correspondingDiagramV2 (kdmoncat)" do
      let
        diagramAstWithSpans = Stbx.parseDiagram diagramSrc -- TODO we don't want spans here
        diagramV2  = DiagramV2.fromDiagram     $  diagramV1Expected
        diagramV2' = DiagramV2.fromDiagramAst <$> diagramAstWithSpans
      pure diagramV2 `shouldEqual` diagramV2'

--------------------------------------------------------------------------------
-- graph DSL
--------------------------------------------------------------------------------

mkAst gelems = List.fromFoldable gelems

mkEdge ss ts = GHyperEdge (HyperEdge unit (List.fromFoldable ss) (List.fromFoldable ts))
