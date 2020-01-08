module GridKit.KeyHandler where

import Prelude

import Data.Foldable (any)
import Data.HeytingAlgebra
import Data.Maybe.First
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.HTML (PlainHTML)
import Halogen.HTML.Events (onKeyDown)
import Halogen.HTML.Properties (IProp)
import Web.Event.Event (stopPropagation, preventDefault)
import Web.UIEvent.KeyboardEvent as K

import Debug.Trace

type Modifiers =
  { ctrlKey  :: Boolean
  , shiftKey :: Boolean
  , altKey   :: Boolean
  , metaKey  :: Boolean
  }

noMods :: Modifiers
noMods = ff

ctrlKey  :: Modifiers
ctrlKey   = noMods { ctrlKey  = true }
shiftKey :: Modifiers
shiftKey  = noMods { shiftKey = true }
altKey   :: Modifiers
altKey    = noMods { altKey   = true }
metaKey  :: Modifiers
metaKey   = noMods { metaKey  = true }

data Key
  = Char String
  | Shortcut Modifiers String

matches :: K.KeyboardEvent -> Key -> Boolean
matches e (Char key) = K.key e == key
matches e (Shortcut modifiers code) =
  K.code e == code &&
  { ctrlKey : K.ctrlKey  e
  , shiftKey: K.shiftKey e
  , altKey  : K.altKey   e
  , metaKey : K.metaKey  e
  } == modifiers

type KeyHandler action =
  { handler :: K.KeyboardEvent -> First action
  , documentation :: Array { keys :: Array Key, description :: PlainHTML }
  }

keyHandler :: ∀ action. Array Key -> PlainHTML -> action -> KeyHandler action
keyHandler keys description action = { handler, documentation: [{ keys, description }] }
  where
    handler keyEvent = guard (any (matches keyEvent) keys) (pure action)

handleKeyDown :: ∀ action r. KeyHandler action -> IProp (onKeyDown :: K.KeyboardEvent | r) action
handleKeyDown { handler } = onKeyDown \keyEvent -> unwrap (handler keyEvent) <#> prevent keyEvent
  where
    prevent keyEvent action = unsafePerformEffect do
      stopPropagation (K.toEvent keyEvent)
      preventDefault (K.toEvent keyEvent)
      pure action

debugKeyCodes :: ∀ action. KeyHandler action
debugKeyCodes =
  { handler: \keyEvent -> trace { key: K.key keyEvent, code: K.code keyEvent } mempty
  , documentation: []
  }
