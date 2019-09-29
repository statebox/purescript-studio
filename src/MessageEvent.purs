-- TODO what should the module name be? it seems to be (at least) an event on Window
module Web.MessageEvent where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Internal.FFI (unsafeReadProtoTagged)
import Web.UIEvent.UIEvent (UIEvent)

-- TODO add targetOrigin and (Maybe transfer), see https://developer.mozilla.org/en-US/docs/Web/API/Window/postMessage
foreign import data MessageEvent :: Type

-- fromUIEvent :: UIEvent -> Maybe MessageEvent
-- fromUIEvent = unsafeReadProtoTagged "MessageEvent"

fromEvent :: Event -> Maybe MessageEvent
fromEvent = unsafeReadProtoTagged "MessageEvent"

-- toUIEvent :: MessageEvent -> UIEvent
-- toUIEvent = unsafeCoerce

toEvent :: MessageEvent -> Event
toEvent = unsafeCoerce

foreign import data_ :: MessageEvent -> String
