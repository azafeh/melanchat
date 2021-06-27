module Shared.IM.Contact where

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Shared.Types
import Shared.IM.Types
import Shared.Unsafe as SU
import Prelude

defaultContact :: Int -> IMUser -> Contact
defaultContact id chatted = {
      available: true,
      shouldFetchChatHistory: false,
      user: chatted,
      impersonating: Nothing,
      chatStarter: id,
      history: [],
      chatAge: 0.0
}

chattingContact :: Array Contact -> Maybe Int -> Contact
chattingContact contacts chatting = SU.fromJust do
      index <- chatting
      contacts !! index