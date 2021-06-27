module Shared.Keydown where

import Prelude

import Data.Maybe (Maybe(..))
import Flame (Key)
import Flame.Html.Event as HA
import Shared.IM.Types
import Flame.Types (NodeData)

import Shared.Unsafe as SU
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent as WUK

keyDownOn :: Key -> (Event -> IMMessage) -> NodeData IMMessage
keyDownOn keyName message = HA.createRawEvent "keydown" handler
      where handler event = do
                  let   keyboardEvent = SU.fromJust $ WUK.fromEvent event
                        key = WUK.key keyboardEvent
                  if key == keyName && not WUK.shiftKey keyboardEvent then
                        pure <<< Just $ message event
                   else
                        pure Nothing
