module Server.Database.Recoveries where

import Type.Proxy (Proxy(..))
import Droplet.Language
import Data.DateTime(DateTime)

type Recoveries = (
      id :: Auto Int,
      uuid :: String,
      created :: Default DateTime,
      active :: Default Boolean,
      recoverer :: Int
)

recoveries :: Table "recoveries" Recoveries
recoveries = Table

_uuid :: Proxy "uuid"
_uuid = Proxy

_created :: Proxy "created"
_created = Proxy

_recoverer :: Proxy "recoverer"
_recoverer = Proxy
