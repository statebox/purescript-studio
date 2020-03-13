module Test.Statebox.Core.WiringTree where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Petrinet.Representation.NLL (ErrNetEncoding(..))
import Statebox.Core.Transition (Glued(..))
import Statebox.Core.WiringTree (LinearizationError(..), WiringTree(..), linearize)

suite :: Spec Unit
suite = do
  describe "WiringTree linearization" do
    it "should fail if the WiringTree contains a Diagram" do
      let diagram = { name:   "diagram"
                    , width:  1
                    , pixels: [1]
                    , names:  ["a"]
                    }
          wiringTree = Diagram diagram []

      linearize wiringTree `shouldEqual` Left DiagramNotYetSupported
    it "should fail if the NLL representation of a net is invalid" do
      let net = { name: "net"
                , names: ["a"]
                , partition: [0]
                , placeNames: Nothing
                }
          wiringTree = Net net

      linearize wiringTree `shouldEqual` Left (NLLDecodingFailed ErrOddLength)
    it "should correctly linearize a net with a single untouched transition" do
      let net = { name: "net"
                , names: ["a"]
                -- 1 -> 2
                , partition: [1, 0, 2, 0]
                , placeNames: Nothing
                }
          wiringTree = Net net
          gluedTransition = Untouched { name: "a"
                                      , path: [0, 0, 0]
                                      , tokens: { pre:  [ { place:  1
                                                          , tokens: 1
                                                          }
                                                        ]
                                                , post: [ { place:  2
                                                          , tokens: 1
                                                          }
                                                        ]
                                                }
                                      , transition: 0
                                      }

      linearize wiringTree `shouldEqual` Right [gluedTransition]
    it "should linearize correctly a net with a single initial transition" do
      let net = { name: "net"
                , names: ["a"]
                -- _ -> 1
                , partition: [0, 1, 0]
                , placeNames: Nothing
                }
          wiringTree = Net net
          gluedTransition = Initial { name: "a"
                                    , path: [0, 0, 0]
                                    , tokens: { pre:  []
                                              , post: [ { place:  1
                                                        , tokens: 1
                                                        }
                                                      ]
                                              }
                                    , transition: 0
                                    }

      linearize wiringTree `shouldEqual` Right [gluedTransition]
    it "should linearize correctly a net with a single final transition" do
      let net = { name: "net"
                , names: ["a"]
                -- 1 -> _
                , partition: [1, 0, 0]
                , placeNames: Nothing
                }
          wiringTree = Net net
          gluedTransition = Final { name: "a"
                                  , path: [0, 0, 0]
                                  , tokens: { pre:  [ { place:  1
                                                      , tokens: 1
                                                      }
                                                    ]
                                            , post: []
                                            }
                                  , transition: 0
                                  }

      linearize wiringTree `shouldEqual` Right [gluedTransition]
    it "should linearize correctly a net with a single transition which is both initial and final" do
      let net = { name: "net"
                , names: ["a"]
                -- _ -> _
                , partition: [0, 0]
                , placeNames: Nothing
                }
          wiringTree = Net net
          gluedTransition = Initial { name: "a"
                                    , path: [0, 0, 0]
                                    , tokens: { pre:  []
                                              , post: []
                                              }
                                    , transition: 0
                                    }

      linearize wiringTree `shouldEqual` Right [gluedTransition]
    it "should linearize correctly a net with multiple transitions" do
      let net = { name: "net"
                , names: ["a", "b", "c", "d"]
                -- a: _  -> 1
                -- b: 1  -> _
                -- c: 11 -> 2
                -- d: _  -> 22
                , partition: [0, 1, 0, 1, 0, 0, 1, 1, 0, 2, 0, 0, 2, 2, 0]
                , placeNames: Nothing
                }
          wiringTree = Net net
          a = Initial { name: "a"
                      , path: [0, 0, 0]
                      , tokens: { pre:  []
                                , post: [ { place:  1
                                          , tokens: 1
                                          }
                                        ]
                                }
                      , transition: 0
                      }
          b = Final { name: "b"
                    , path: [0, 0, 0]
                    , tokens: { pre:  [ { place:  1
                                        , tokens: 1
                                        }
                                      ]
                              , post: []
                              }
                    , transition: 1
                    }
          c = Untouched { name: "c"
                        , path: [0, 0, 0]
                        , tokens: { pre:  [ { place:  1
                                            , tokens: 2
                                            }
                                          ]
                                  , post: [ { place:  2
                                            , tokens: 1
                                            }
                                          ]
                                  }
                        , transition: 2
                        }
          d = Initial { name: "d"
                      , path: [0, 0, 0]
                      , tokens: { pre:  []
                                , post: [ { place:  2
                                          , tokens: 2
                                          }
                                        ]
                                }
                      , transition: 3
                      }

      linearize wiringTree `shouldEqual` Right [a, d, c, b]
