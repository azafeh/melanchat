module Server.Database.Messages where

import Type.Proxy (Proxy(..))
import Droplet.Language
import Data.DateTime(DateTime)
import Shared.IM.Types
import Shared.Types
import Data.Maybe(Maybe(..))

type Messages = (
      id :: Auto Int,
      temporary_id :: Int,
      sender :: Int,
      recipient :: Int,
      date :: Default DateTime,
      content :: String,
      status :: MessageStatus,
      visualized :: Maybe DateTime
)

messages :: Table "messages" Messages
messages = Table

_temporary_id :: Proxy "temporary_id"
_temporary_id = Proxy

_sender :: Proxy "sender"
_sender = Proxy

_recipient :: Proxy "recipient"
_recipient = Proxy

_content :: Proxy "content"
_content = Proxy

_status :: Proxy "status"
_status = Proxy

_visualized :: Proxy "visualized"
_visualized = Proxy

