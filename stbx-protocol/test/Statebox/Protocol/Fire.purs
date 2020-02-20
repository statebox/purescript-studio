module Test.Statebox.Protocol.Fire where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Petrinet.Representation.Marking (fromFoldable)
import Data.Petrinet.Representation.NLL (ErrNetEncoding(..))
import Statebox.Core.Firing (Firing)
import Statebox.Core.Wiring (NetsAndDiagramsIndex(..))
import Statebox.Core.WiringTree (LinearizationError(..))
import Statebox.Protocol.Fire (FiringError(..), fire, fireMultiple)

firing :: Int -> Firing
firing i = { execution: Nothing
           , message: Nothing
           , path: NonEmpty i []
           }

suite :: Spec Unit
suite = do
  describe "Firing" do
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
          marking = mempty
      fire wiring marking (firing 3) `shouldEqual` Left FireInvalidWiringTree
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
          marking = mempty
      fire wiring marking (firing 3) `shouldEqual` Left (FireLinearizationError (NLLDecodingFailed ErrOddLength))
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
          marking = mempty
      fire wiring marking (firing 3) `shouldEqual` Left FireTransitionIndexOutOfBounds
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
          marking = mempty
      fire wiring marking (firing 0) `shouldEqual` Left FireTransitionNotEnabled
    it "succeeds for an initial transition" do
      let wiring = { diagrams: [{ name: "diagram"
                                , names: []
                                , pixels: [1]
                                , width: 1
                                }]
                   , nets: [{ name: "net"
                            , names: ["a"]
                            -- _ -> 1
                            , partition: [0, 1, 0]
                            , placeNames: Nothing
                            }]
                   , labels: [NetsAndDiagramsIndex 0]
                   }
          marking = mempty
      fire wiring marking (firing 0) `shouldEqual` Right (fromFoldable [1 /\ 1])
    it "succeeds for a normal transition" do
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
          marking = fromFoldable [1 /\ 1]
      fire wiring marking (firing 0) `shouldEqual` Right (fromFoldable [2 /\ 1])
    it "succeeds for a terminal transition" do
      let wiring = { diagrams: [{ name: "diagram"
                                , names: []
                                , pixels: [1]
                                , width: 1
                                }]
                   , nets: [{ name: "net"
                            , names: ["a"]
                            -- 1 -> _
                            , partition: [1, 0, 0]
                            , placeNames: Nothing
                            }]
                   , labels: [NetsAndDiagramsIndex 0]
                   }
          marking = fromFoldable [1 /\ 1]
      fire wiring marking (firing 0) `shouldEqual` Right mempty
  describe "Firing multiple transitions" do
    it "fails if one of the transitions is not enabled" do
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
          marking = fromFoldable [1 /\ 1]
      fireMultiple wiring marking [firing 0, firing 0] `shouldEqual` Left FireTransitionNotEnabled
    it "suceeds if all of the transitions are enabled" do
      let wiring = { diagrams: [{ name: "diagram"
                                , names: []
                                , pixels: [1]
                                , width: 1
                                }]
                   , nets: [{ name: "net"
                            , names: ["a", "b", "c", "d"]
                            -- 1 -> 2 -> 3 -> 5
                            --      |
                            --      ---> 4 -> 6
                            , partition: [1, 0, 2, 0, 2, 0, 3, 4, 0, 3, 0, 5, 0, 4, 0, 6, 0]
                            , placeNames: Nothing
                            }]
                   , labels: [NetsAndDiagramsIndex 0]
                   }
          marking = fromFoldable [1 /\ 1]
      fireMultiple wiring marking [firing 0, firing 1, firing 2, firing 3] `shouldEqual` Right (fromFoldable [5 /\ 1, 6 /\ 1])
