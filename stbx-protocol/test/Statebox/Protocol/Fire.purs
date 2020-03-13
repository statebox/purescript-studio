module Test.Statebox.Protocol.Fire where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Petrinet.Representation.Marking (fromFoldable) as Marking
import Data.Petrinet.Representation.NLL (ErrNetEncoding(..))
import Statebox.Core.Firing (Firing)
import Statebox.Core.Wiring (NetsAndDiagramsIndex(..), GluedTransitionIdRaw)
import Statebox.Core.WiringTree (LinearizationError(..))
import Statebox.Protocol.Fire (FiringError(..), fire, fireMultiple)

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
      fire wiring marking (mkFiring 3) `shouldEqual` Left FireInvalidWiringTree

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
      fire wiring marking (mkFiring 3) `shouldEqual` Left (FireLinearizationError (NLLDecodingFailed ErrOddLength))

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
      fire wiring marking (mkFiring 3) `shouldEqual` Left FireTransitionIndexOutOfBounds

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
      fire wiring marking (mkFiring 0) `shouldEqual` Left FireTransitionNotEnabled

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
      fire wiring marking (mkFiring 0) `shouldEqual` Right (Marking.fromFoldable [1 /\ 1])

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
          marking = Marking.fromFoldable [1 /\ 1]
      fire wiring marking (mkFiring 0) `shouldEqual` Right (Marking.fromFoldable [2 /\ 1])

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
          marking = Marking.fromFoldable [1 /\ 1]
      fire wiring marking (mkFiring 0) `shouldEqual` Right mempty

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
          marking = Marking.fromFoldable [1 /\ 1]
      fireMultiple wiring marking (mkFiring <$> [0, 0]) `shouldEqual` Left FireTransitionNotEnabled

    it "succeeds if all of the transitions are enabled" do
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
          marking = Marking.fromFoldable [1 /\ 1]
      fireMultiple wiring marking (mkFiring <$> [0, 1, 2, 3]) `shouldEqual` Right (Marking.fromFoldable [5 /\ 1, 6 /\ 1])

--------------------------------------------------------------------------------

mkFiring :: GluedTransitionIdRaw -> Firing
mkFiring tid =
  { execution: Nothing
  , message: Nothing
  , path: NonEmpty tid []
  }
