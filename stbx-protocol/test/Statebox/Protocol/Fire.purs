module Test.Statebox.Protocol.Fire where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Statebox.Core.Wiring (NetsAndDiagramsIndex(..))
import Statebox.Protocol.Fire (FiringError(..), fire)

suite :: Spec Unit
suite = do
  describe "Firing a firing" do
    it "fails if the wiring is not valid" do
      -- the only relevant thing here is that the wiring is not valid
      let wiring = { diagrams: [{ name: "diagram"
                                , names: []
                                , pixels: [1, 2]
                                , width: 1
                                }]
                   , nets : []
                   , labels : [NetsAndDiagramsIndex 5]
                   }
          firing = { execution : Nothing
                   , message: Nothing
                   , path: NonEmpty 3 []
                   }
          marking = mempty
      fire wiring firing marking `shouldEqual` Left FireInvalidWiringTree
