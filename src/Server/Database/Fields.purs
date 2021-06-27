module Server.Database.Fields where

import Type.Proxy(Proxy(..))

_id :: Proxy "id"
_id = Proxy

_active :: Proxy "active"
_active = Proxy

_name :: Proxy "name"
_name = Proxy

_date :: Proxy "date"
_date = Proxy

c :: Proxy "c"
c = Proxy