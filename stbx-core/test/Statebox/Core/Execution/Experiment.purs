module Test.Statebox.Core.Execution.Experiment where

import Prelude
import Control.Monad.State

-- import Data.Array (length, (..))
import Data.Foldable (foldl, foldr)
import Data.Maybe (Maybe(..)) -- , fromMaybe, maybe)

import Statebox.Core.Execution (GluedMarking, StbxObj, Marked)
import Statebox.Core (decode) as Stbx
import Statebox.Core.Execution as Stbx
import Statebox.Core.Types (GluedTransitionId(..), TID, Wiring)
-- import Statebox.Core.Transaction (HashStr, TxSum, FiringTx, WiringTx, isExecution)

import Effect.Class (liftEffect)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Console (write)
import Debug.Trace (spy)



-- TODO when applying it using (modify fire 3) we want the TID first
-- fire :: StbxObj -> TID -> Maybe Marked
-- fire' :: GluedTransitionId -> StbxObj -> Maybe StbxObj
fire' :: TID -> StbxObj -> Maybe StbxObj
fire' tid s = map Stbx.fromMarked $ Stbx.fire s tid

-- -- state type: GluedMarking? StbxObj? anders?
-- x1 :: forall m. StateT GluedMarking _
-- x1 = do
--   modify Stbx.fire

--------------------------------------------------------------------------------

-- hmm, perhaps monadic proggies like this would be convenient if we're
-- *generating* code from a statebox protocol, but i'm not sure how useful this
-- is for our use case here

-- glued TID?
-- enabledTIDs :: forall m. StateT (Maybe StbxObj) m (Array TID)
enabledTIDs :: State (Maybe StbxObj) (Array TID)
enabledTIDs = do
  s0 <- get
  let s1 = fire'' 3 s0
  put s1

  s2 <- modify $ fire'' 2
  pure [42]
  where
    -- get inside the Maybe
    fire'' tid soM = soM >>= fire' tid

--------------------------------------------------------------------------------

-- TODO TxSum or sth?
execTrace1_txs :: Array _
execTrace1_txs = []

-- TODO glued tid?
execTrace1_tids :: Array TID
execTrace1_tids = [3, 2, 4, 1]

-- from tx view code ------------------------------------------------------------------------------

type GluedTID = TID
-- TODO should really be (Set GluedTID) instead of Array
type NetState = { marking :: Unit, enabled :: Array GluedTID }

-- computeNetState :: Array GluedTID -> NetState
computeNetState :: Array GluedTID -> NetState
computeNetState tids = foldr (\tid acc -> { marking: unit
                                          , enabled: acc.enabled <> [tid]
                                          })
                             { marking: unit, enabled: [] }
                             tids

-- what to do in case of failing fire?
-- - record in history log which firings were successful
-- - have the state be non-optional? but with indications re up to which firing it was successful?

type NetState' =
  { stbxObjM :: Maybe StbxObj -- TODO either? we want to give reasons for failure, eg transition doesnt exist or not enabled
  , log      :: Array TID -- we also want to log markings
  }

-- type LogEntry = { tid :: TID, marking :: ... }

foldrNetState' :: StbxObj -> Array GluedTID -> NetState'
foldrNetState' initialState tids =
  foldr (\tid previousState ->
           { stbxObjM: fire' tid =<< previousState.stbxObjM
           , log:      previousState.log <> [tid]
           }
        )
        (mkState initialState)
        tids
  where
    mkState :: StbxObj -> NetState'
    mkState o = { stbxObjM: pure o, log: mempty }

foldlNetState' :: StbxObj -> Array GluedTID -> NetState'
foldlNetState' initialState tids =
  foldl (\previousState tid ->
           { stbxObjM: fire' tid =<< previousState.stbxObjM
           , log:      previousState.log <> [tid]
           }
        )
        (mkState initialState)
        tids
  where
    mkState :: StbxObj -> NetState'
    mkState o = { stbxObjM: pure o, log: mempty }

--------------------------------------------------------------------------------

-- TODO dedupe
wiring1 :: Wiring
wiring1 =
  { nets: [ { name: "a"
            , partition: [0,1,0,1,0,0]
            , names: ["t","u"]
            , placeNames: Nothing
            }
          ]
  , diagrams: [ { name: "z"
                , width: 1
                , pixels: [1,2]
                , names: ["m","n"]
                }
              ]
  , labels: [0,0]
  }

suite :: Spec Unit
suite = do
  let
    s0                 = spy "s0"              $ Stbx.fromWiring wiring1
  describe "Stbx" do
    it "koekjedepoekje" do
      -- so <- liftEffect $ Stbx.decode "0a20dce4021c8f117e89c479665f6d61ff650b150af375d6498b593da6afa8d2ca9f1afa010add010a0a70726976696c656467651001100010021000100210001006100010011000100310001003100010011000100210001004100010031000100510001004100010051000100110001005100010021000100510001006100010021000100610001003100010061000100510001000100310001a036275791a07666f7253616c651a05626c6f636b1a07756e626c6f636b1a047363616e1a086e6f74536f6c64321a0873686f774f7665721a076e6f74536f6c641a066e6f53686f771a04627579271a076275794261636b1a096e6f745265736f6c641a0663726561746512160a046d61696e10011801220a70726976696c656467651800"

      let
        -- s0                 = spy "s0"              $ Stbx.fromWiring wiring1
        transitions        = spy "transitions"     $ Stbx.gluedTransitions s0
        transitionCount    = spy "transitionCount" $ Stbx.transitionCount s0
        transitionIds      = spy "transitionIds"   $ Stbx.transitionIds s0

        e0 = spy "e0" $ Stbx.enabledMaybe s0 0
        e1 = spy "e1" $ Stbx.enabledMaybe s0 1
        e2 = spy "e2" $ Stbx.enabledMaybe s0 2
        es = spy "es" $ Stbx.enabledTransitionIds s0

      liftEffect $ write $ show e0
      liftEffect $ write $ show e1
      liftEffect $ write $ show e2
      liftEffect $ write $ show es

      -- liftEffect $ write "hoi poes"
      let s0Dump = spy "s0" s0

      es `shouldEqual` (GluedTransitionId <$> [0])

    it "should produce expected results from sequence of firings #1" do
      let s0sl = spy "s0s" $ foldlNetState' s0 $ []
      let s1sl = spy "s1s" $ foldlNetState' s0 $ [0]
      let s2sl = spy "s2s" $ foldlNetState' s0 $ [0,1]
      let s3sl = spy "s3s" $ foldlNetState' s0 $ [0,1,2]

-- TODO oh right, wait, bc we do foldR instead of foldL this should be flipped too
      let s0s = spy "s0s" $ foldrNetState' s0 $ [2,1,0]

      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "marking" $ Stbx.marking <$> s0s.stbxObjM
      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "enabled trs" $ Stbx.enabledTransitionIds <$> s0s.stbxObjM
      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "marking s0sl" $ Stbx.marking <$> s0sl.stbxObjM
      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "marking s1sl" $ Stbx.marking <$> s1sl.stbxObjM
      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "marking s2sl" $ Stbx.marking <$> s2sl.stbxObjM
      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "marking s3sl" $ Stbx.marking <$> s3sl.stbxObjM
      s0s.log `shouldEqual` ([0,1,2])

      -- let decodedJsonString = Stbx.stbxObjToJsonString decodedStbxObj
      -- 1  `shouldEqual` 2

    it "should produce expected results from sequence of firings #1" do
      let s0sl = spy "s0s" $ foldlNetState' s0 $ []
      let s1sl = spy "s1s" $ foldlNetState' s0 $ [0,0,0]
      let s2sl = spy "s2s" $ foldlNetState' s0 $ [0,0,0,1]
      let s3sl = spy "s3s" $ foldlNetState' s0 $ [0,0,0,1,1]

-- TODO oh right, wait, bc we do foldR instead of foldL this should be flipped too
      let s0s = spy "s0s" $ foldrNetState' s0 $ [2,1,0]

      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "marking" $ Stbx.marking <$> s0s.stbxObjM
      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "enabled trs" $ Stbx.enabledTransitionIds <$> s0s.stbxObjM
      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "marking s0sl" $ Stbx.marking <$> s0sl.stbxObjM
      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "marking s1sl" $ Stbx.marking <$> s1sl.stbxObjM
      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "marking s2sl" $ Stbx.marking <$> s2sl.stbxObjM
      liftEffect $ write "\n"
      liftEffect $ write $ show $ spy "marking s3sl" $ Stbx.marking <$> s3sl.stbxObjM
      s0s.log `shouldEqual` ([0,1,2])

      -- let decodedJsonString = Stbx.stbxObjToJsonString decodedStbxObj
      -- 1  `shouldEqual` 2
