module Test.Main where

import Prelude
import Data.Maybe (Maybe)
import Data.Petrinet.Representation.Dict
import Effect (Effect)
import Effect.Class.Console (log)

import ExampleData as Ex

main :: Effect Unit
main = do
  log $ "pre  =     " <> (show $ preMarking                            <$> transition 100)
  log $ "post =     " <> (show $ postMarking                           <$> transition 100)
  log $ "sum  =     " <> (show $ (\t -> postMarking t <> preMarking t) <$> transition 100)
  log $ "m    =     " <> (show $ Ex.net1.marking)
  log $ "m'   =     " <> (show $ fireAtMarking Ex.net1.marking         <$> transition 100)
  where
    transition :: Ex.TID -> Maybe _
    transition = Ex.net1.findTransition
