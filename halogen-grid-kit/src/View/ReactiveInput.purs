module View.ReactiveInput where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)
import Data.Maybe
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Query.HalogenM as H

type State input state =
  { input :: input
  , rest :: state
  }

data Action input action
  = Initialize
  | Update input
  | Rest action

type ComponentSpec surface state action slots input output m =
  { initialState :: state
  , render :: input -> state -> surface (H.ComponentSlot surface slots m action) action
  , handleInput :: input -> H.HalogenM state action slots output m Unit
  , handleAction :: action -> H.HalogenM state action slots output m Unit
  }

mkComponent
  :: ∀ surface state query action slots input output m
   . MonadEffect m
  => Bifunctor surface
  => ComponentSpec surface state action slots input output m
  -> H.Component surface query input output m
mkComponent spec@{ initialState, render } =
  H.mkComponent
    { initialState: { input: _, rest: initialState }
    , render: \{ input, rest } -> render input rest # mapAction Rest
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handle spec
      , receive = Just <<< Update
      , initialize = Just Initialize
      }
    }

handle
  :: ∀ surface state action slots input output m
   . MonadEffect m
  => ComponentSpec surface state action slots input output m
  -> Action input action
  -> H.HalogenM (State input state) (Action input action) slots output m Unit
handle { handleInput, handleAction } = case _ of

  Initialize -> do
    { input } <- H.get
    mapHalogenM $ handleInput input

  Update newInput -> do
    { input } <- H.get
    H.modify_ _ { input = newInput }
    mapHalogenM $ handleInput newInput

  Rest action ->
    mapHalogenM $ handleAction action


mapHalogenM
  :: ∀ state action slots input output m. MonadEffect m
  => H.HalogenM state action slots output m Unit
  -> H.HalogenM (State input state) (Action input action) slots output m Unit
mapHalogenM h = do
  { input } <- H.get
  h # H.mapAction Rest
    # H.imapState { input, rest: _ } _.rest

mapAction
  :: ∀ surface slots m a b
   . Bifunctor surface
  => (a -> b)
  -> surface (H.ComponentSlot surface slots m a) a
  -> surface (H.ComponentSlot surface slots m b) b
mapAction f = bimap (map f) f
