module GridKit.KeyHandler where

import Prelude hiding (div)

import Data.Foldable (any, intercalate, foldMap)
import Data.HeytingAlgebra
import Data.Maybe
import Data.Maybe.First
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Vec3 (Vec3, vec2)
import Effect.Class (class MonadEffect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.HTML hiding (prop, map)
import Halogen.HTML.Events (onKeyDown, onClick)
import Halogen.HTML.Properties (IProp, classes)
import Web.Event.Event (stopPropagation, preventDefault)
import Web.UIEvent.KeyboardEvent as K

import Debug.Trace

type KeysWithHelpPopup action =
  { onKeyDown :: ∀ r. IProp (onKeyDown :: K.KeyboardEvent | r) action
  , helpPopup :: ∀ m c. MonadEffect m => Boolean -> ComponentHTML action c m
  }

keysWithHelpPopup
  :: ∀ action
   . { keys :: KeyHandler action
     , popupAction :: action }
  -> KeysWithHelpPopup action
keysWithHelpPopup { keys, popupAction } =
  { onKeyDown: handleKeyDown (keys <> keyHandler [ Char "?" ] Nothing popupAction)
  , helpPopup: renderKeyHelpPopup keys popupAction
  }

type Modifiers =
  { ctrl  :: Boolean
  , shift :: Boolean
  , alt   :: Boolean
  , meta  :: Boolean
  }

noMods :: Modifiers
noMods = ff

ctrlKey  :: Modifiers
ctrlKey   = noMods { ctrl  = true }

shiftKey :: Modifiers
shiftKey  = noMods { shift = true }

altKey   :: Modifiers
altKey    = noMods { alt   = true }

metaKey  :: Modifiers
metaKey   = noMods { meta  = true }

data Key
  = Char String
  | Shortcut Modifiers String

matches :: K.KeyboardEvent -> Key -> Boolean
matches e (Char key) = K.key e == key
matches e (Shortcut modifiers code) =
  K.code e == code &&
  { ctrl : K.ctrlKey  e
  , shift: K.shiftKey e
  , alt  : K.altKey   e
  , meta : K.metaKey  e
  } == modifiers

type KeyDocumentation = Array
  { keys :: Array Key
  , description :: PlainHTML
  }

type KeyHandler action =
  { handler :: K.KeyboardEvent -> First action
  , documentation :: KeyDocumentation
  }

keyHandler :: ∀ action. Array Key -> Maybe PlainHTML -> action -> KeyHandler action
keyHandler keys descriptionM action = { handler, documentation: descriptionM # foldMap \description -> [{ keys, description }] }
  where
    handler keyEvent = guard (any (matches keyEvent) keys) (pure action)

cursorKeyHandler :: ∀ action. Modifiers -> (Vec3 Int -> action) -> KeyHandler action
cursorKeyHandler mods mkAction
   = keyHandler [ Shortcut mods "ArrowLeft"  ] Nothing (mkAction (vec2 (-1)  0 ))
  <> keyHandler [ Shortcut mods "ArrowUp"    ] Nothing (mkAction (vec2   0 (-1)))
  <> keyHandler [ Shortcut mods "ArrowRight" ] Nothing (mkAction (vec2   1   0 ))
  <> keyHandler [ Shortcut mods "ArrowDown"  ] Nothing (mkAction (vec2   0   1 ))

debugKeyCodes :: ∀ action. DebugWarning => KeyHandler action
debugKeyCodes =
  { handler: \keyEvent -> trace { key: K.key keyEvent, code: K.code keyEvent } mempty
  , documentation: []
  }

handleKeyDown :: ∀ action r. KeyHandler action -> IProp (onKeyDown :: K.KeyboardEvent | r) action
handleKeyDown { handler } = onKeyDown \keyEvent -> unwrap (handler keyEvent) <#> prevent keyEvent
  where
    prevent keyEvent action = unsafePerformEffect do
      stopPropagation (K.toEvent keyEvent)
      preventDefault (K.toEvent keyEvent)
      pure action

displayKeyCode :: String -> String
displayKeyCode "Minus" = "-"
displayKeyCode "Equal" = "+"
displayKeyCode k = k

renderKeyHelpPopup
  :: ∀ action m c. MonadEffect m
  => KeyHandler action -> action -> Boolean
  -> ComponentHTML action c m
renderKeyHelpPopup { documentation } toggleAction visible = div_ $ guard visible
  [ div [ classes [ClassName "key-help-popup-container"] ]
        [ div [ classes [ClassName "key-help-popup-backdrop" ], onClick $ \_ -> Just toggleAction ] []
        , div [ classes [ClassName "key-help-popup"] ]
              [ h3_ [ text "Keyboard shortcuts" ]
              , ul_ $ documentation <#> \{ keys, description } -> li_
                    [ span [ classes [ClassName "key-help-popup-keys"] ] $ renderKeys keys
                    , description # fromPlainHTML
                    ]
              ]
        ]
  ]
  where
    renderKeys = map renderKey >>> intercalate [text " or "]
    renderKey (Char c) = [ kbd_ [text c] ]
    renderKey (Shortcut { ctrl, alt, shift, meta } code) = [ kbd_ [ text $
      guard ctrl "Ctrl " <> guard alt "Alt " <> guard shift "Shift " <> guard meta "Command " <> displayKeyCode code
    ] ]
