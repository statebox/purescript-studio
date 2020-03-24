module View.CRUDAction where

import Prelude

import Control.Monad.State.Class
import Data.Lens ((.=), preview, Optic)
import Data.Lens.At
import Data.Maybe
import Data.Profunctor.Choice
import Data.Profunctor.Strong
import Data.String (drop)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (random)

data CRUDAction a
  = CreateAction a
  | UpdateAction (a -> a) String
  | DeleteAction String

type Affine s t a b = forall p. Strong p => Choice p => Optic p s t a b
type Affine' s a = Affine s s a a

handleCRUDAction
  :: ∀ m s t a. MonadEffect m => MonadState s m => At t String a
  => Affine' s t -> CRUDAction a -> (String -> Maybe a -> m Unit) -> m Unit
handleCRUDAction l action eventHandler =
  case action of
    CreateAction a -> do
      rnd <- liftEffect random
      let id = "id" <> drop 2 (show rnd)
      handle id $ Just a
    UpdateAction f id -> do
      s <- get
      handle id (map f (join (preview (l <<< at id) s)))
    DeleteAction id -> do
      handle id Nothing
  where
    handle :: String -> Maybe a -> m Unit
    handle id mValue = do
      l <<< at id .= mValue
      eventHandler id mValue
