module Test.Statebox.Protocol.Fire where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Petrinet.Representation.NLL (ErrNetEncoding(..))
import Statebox.Core.Wiring (NetsAndDiagramsIndex(..))
import Statebox.Core.WiringTree (LinearizationError(..))
import Statebox.Protocol.Fire (FiringError(..), fire)

suite :: Spec Unit
suite = do
  describe "Firing a firing" do
    it "fails if the wiring is not valid" do
      -- the only relevant thing here is that the wiring is not valid
      let wiring = { diagrams: [{ name: "diagram"
                                , names: []
                                , pixels: [1]
                                , width: 1
                                }]
                   , nets: []
                   , labels: [NetsAndDiagramsIndex 5]
                   }
          firing = { execution: Nothing
                   , message: Nothing
                   , path: NonEmpty 3 []
                   }
          marking = mempty
      fire wiring firing marking `shouldEqual` Left FireInvalidWiringTree
    it "fails if the linearization of the wiring fails" do
      -- the only relevant thing here is that the wiring is not valid
      let wiring = { diagrams: [{ name: "diagram"
                                , names: []
                                , pixels: [1]
                                , width: 1
                                }]
                   , nets: [{ name: "net"
                            , names: ["a"]
                            , partition: [0]
                            , placeNames: Nothing
                            }]
                   , labels: [NetsAndDiagramsIndex 0]
                   }
          firing = { execution: Nothing
                   , message: Nothing
                   , path: NonEmpty 3 []
                   }
          marking = mempty
      fire wiring firing marking `shouldEqual` Left (FireLinearizationError (NLLDecodingFailed ErrOddLength))
    it "fails if the firing path is out of bounds" do
      let wiring = { diagrams: [{ name: "diagram"
                                , names: []
                                , pixels: [1]
                                , width: 1
                                }]
                   , nets: [{ name: "net"
                            , names: ["a"]
                            -- 1 -> 2
                            , partition: [1, 0, 2, 0]
                            , placeNames: Nothing
                            }]
                   , labels: [NetsAndDiagramsIndex 0]
                   }
          firing = { execution: Nothing
                   , message: Nothing
                   , path: NonEmpty 3 []
                   }
          marking = mempty
      fire wiring firing marking `shouldEqual` Left FireTransitionIndexOutOfBounds
    it "fails if the selected transition is not enabled" do
      let wiring = { diagrams: [{ name: "diagram"
                                , names: []
                                , pixels: [1]
                                , width: 1
                                }]
                   , nets: [{ name: "net"
                            , names: ["a"]
                            -- 1 -> 2
                            , partition: [1, 0, 2, 0]
                            , placeNames: Nothing
                            }]
                   , labels: [NetsAndDiagramsIndex 0]
                   }
          firing = { execution: Nothing
                   , message: Nothing
                   , path: NonEmpty 0 []
                   }
          marking = mempty
      fire wiring firing marking `shouldEqual` Left FireTransitionNotEnabled
