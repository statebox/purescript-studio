module Test.Statebox.FFI where

import Prelude

import Data.Array (length, (..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)

import Debug.Trace (spy)
import Test.Spec (Spec, pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Statebox.FFI
import Statebox.FFI as Stbx
import Statebox.API.Types (Wiring)

suite :: Spec Unit
suite = do
  describe "Protocol executions" do
    let
      -- | Gluing `s` and `t` into a single net `s&t`:
      -- | ```
      -- |        s =         t=
      -- |      _     _      _     _
      -- |     |_|-O-|_|    |_|-O-|_|
      -- |      x     y      x     y
      -- |
      -- |      _     ________     _
      -- |     |_|-O-|________|-O-|_|
      -- |     s.x       x&y      t.y
      -- | ```
      wiring :: Wiring
      wiring =
        { nets: [ { name: "a"
                  , partition: [0,1,0,1,0,0]
                  , names: ["x","y"]
                  -- , placeNames: Nothing
                  }
                ]
        , diagrams: [ { name: "z"
                      , width: 1
                      , pixels: [1,2]
                      , names: ["s","t"]
                      }
                    ]
        , labels: [0,0]
        }

      s1                 = spy "s1"              $ Stbx.fromWiring wiring
      transitions        = spy "transitions"     $ Stbx.gluedTransitions s1
      transitionCount    = spy "transitionCount" $ Stbx.transitionCount s1
      transitionIds      = spy "transitionIds"   $ Stbx.transitionIds s1

    it "should interpret glued net topology correctly" do
      let
        gluedTransition0 = spy "transition 0" $ Stbx.getGluedTransition s1 (GluedTransitionId 0)
        gluedTransition1 = spy "transition 1" $ Stbx.getGluedTransition s1 (GluedTransitionId 1)
        gluedTransition2 = spy "transition 2" $ Stbx.getGluedTransition s1 (GluedTransitionId 2)
        gts0             = spy "gts 0"        $ (fromTransitionSortString <<<_.sort) <$> Stbx.getGluedTransition s1 (GluedTransitionId 0)

      transitionCount `shouldEqual` 3
      transitionIds   `shouldEqual` [ GluedTransitionId 0, GluedTransitionId 1, GluedTransitionId 2]

      (gluedTransition0 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Initial
      (gluedTransition1 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Glued
      (gluedTransition2 >>= (_.sort >>> fromTransitionSortString)) `shouldEqual` Just Final

    it "should interpret further glued net topology correctly" do
      let
        firing0 = spy "firing 0" $ fromGluedTransition2JS <$> Stbx.getFiring s1 (GluedTransitionId 0)
        firing1 = spy "firing 1" $ fromGluedTransition2JS <$> Stbx.getFiring s1 (GluedTransitionId 1)
        firing2 = spy "firing 2" $ fromGluedTransition2JS <$> Stbx.getFiring s1 (GluedTransitionId 2)

      firing2 `shouldEqual` Just { pre: [ [ 1 ], []    ], post: "y"   }
      firing1 `shouldEqual` Just { pre: [ [ 1 ], [ 1 ] ], post: "y&x" }
      firing0 `shouldEqual` Just { pre: [ []   , [ 1 ] ], post: "x"   }

    it "should be well-behaved at non-existing transitions" do
      let
        err1               = spy "enabledMaybe 666"   $ Stbx.enabledMaybe s1 666
        err2               = spy "enabled 666"        $ Stbx.enabled      s1 666

      err1 `shouldEqual` Nothing
      err2 `shouldEqual` false

    it "initial net state should be interpreted correctly" do
      let
        enabled1           = spy "enabled s1"         $ Stbx.enabled s1 0
        enabled1M          = spy "enabled s1"         $ Stbx.enabledMaybe s1 0
        enabledTrsMaybe1   = spy "enabledTrs s1"      $ Stbx.enabledMaybe_glued s1 <$> transitionIds

        marking1           = spy "marking 1"          $ Stbx.marking s1

      marking1         `shouldEqual` []
      enabledTrsMaybe1 `shouldEqual` [Just true, Just false, Just false]
      enabled1M        `shouldEqual` (Just true)

    it "net state after firing transition 0" do
      let
        marked2            = spy "marked2 after fire" $ Stbx.fire s1 0
        marking2           = spy "marking 2"          $ Stbx.marking s2
        s2Maybe            = spy "s2Maybe after fire" $ Stbx.fromMarked <$> marked2
        s2                 = spy "s2      after fire" $ fromMaybe s1 s2Maybe
        enabledTrsMaybe2   = spy "enabledTrs s2"      $ Stbx.enabledMaybe_glued s2 <$> transitionIds

      marking2         `shouldEqual` [{ path: [0], marking: [1] }]
      enabledTrsMaybe2 `shouldEqual` [Just false, Just true, Just false]
