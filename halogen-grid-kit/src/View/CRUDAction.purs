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

data CRUDAction k a
  = CreateAction a
  | UpdateAction (a -> a) k
  | DeleteAction k

-- TODO: remove next 2 lines if a new version of purescript-profunctor-lenses is published
type Affine s t a b = forall p. Strong p => Choice p => Optic p s t a b
type Affine' s a = Affine s s a a

handleCRUDAction
  :: ∀ m s t k a. MonadState s m => At t k a
  => m k -> Affine' s t -> CRUDAction k a -> (k -> Maybe a -> m Unit) -> m Unit
handleCRUDAction genId l action eventHandler =
  case action of
    CreateAction a -> do
      id <- genId
      handle id $ Just a
    UpdateAction f id -> do
      s <- get
      handle id (map f (join (preview (l <<< at id) s)))
    DeleteAction id -> do
      handle id Nothing
  where
    handle :: k -> Maybe a -> m Unit
    handle id mValue = do
      l <<< at id .= mValue
      eventHandler id mValue

getRandomId :: ∀ m. MonadEffect m => String -> m String
getRandomId prefix = do
  rnd <- liftEffect random
  pure $ prefix <> drop 2 (show rnd)
