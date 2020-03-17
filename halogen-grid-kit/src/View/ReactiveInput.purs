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

type ComponentSpecRow surface state action slots input output m r =
  ( initialState :: state
  , render :: input -> state -> surface (H.ComponentSlot surface slots m action) action
  , handleInput :: input -> H.HalogenM state action slots output m Unit
  , handleAction :: input -> action -> H.HalogenM state action slots output m Unit
  | r
  )

type ComponentSpec surface state action slots input output m =
  Record (ComponentSpecRow surface state action slots input output m ())

type ComponentSpecQuery surface state query action slots input output m =
  Record (ComponentSpecRow surface state action slots input output m
         (handleQuery :: forall a. query a -> H.HalogenM state action slots output m (Maybe a)))

mkComponentWithQuery
  :: ∀ surface state query action slots input output m
   . MonadEffect m
  => Bifunctor surface
  => ComponentSpecQuery surface state query action slots input output m
  -> H.Component surface query input output m
mkComponentWithQuery spec@{ initialState, render } =
  H.mkComponent
    { initialState: { input: _, rest: initialState }
    , render: \{ input, rest } -> render input rest # mapAction Rest
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handle spec
      , handleQuery = \q -> H.get >>= \{ input } -> mapHalogenM input (spec.handleQuery q)
      , receive = Just <<< Update
      , initialize = Just Initialize
      }
    }

mkComponent
  :: ∀ surface state query action slots input output m
   . MonadEffect m
  => Bifunctor surface
  => ComponentSpec surface state action slots input output m
  -> H.Component surface query input output m
mkComponent { initialState, render, handleInput, handleAction } =
  mkComponentWithQuery { initialState, render, handleInput, handleAction, handleQuery: H.defaultEval.handleQuery }

handle
  :: ∀ surface state query action slots input output m
   . MonadEffect m
  => ComponentSpecQuery surface state query action slots input output m
  -> Action input action
  -> H.HalogenM (State input state) (Action input action) slots output m Unit
handle { handleInput, handleAction } = case _ of

  Initialize -> do
    { input } <- H.get
    mapHalogenM input $ handleInput input

  Update newInput -> do
    H.modify_ _ { input = newInput }
    mapHalogenM newInput $ handleInput newInput
    H.modify_ _ { input = newInput }

  Rest action -> do
    { input } <- H.get
    mapHalogenM input $ handleAction input action


mapHalogenM
  :: ∀ state action slots input output m r. MonadEffect m
  => input -> H.HalogenM state action slots output m r
  -> H.HalogenM (State input state) (Action input action) slots output m r
mapHalogenM input h = do
  h # H.mapAction Rest
    # H.imapState { input, rest: _ } _.rest

mapAction
  :: ∀ surface slots m a b
   . Bifunctor surface
  => (a -> b)
  -> surface (H.ComponentSlot surface slots m a) a
  -> surface (H.ComponentSlot surface slots m b) b
mapAction f = bimap (map f) f
