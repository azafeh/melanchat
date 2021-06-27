module Server.Database.Experiments  where

import Droplet.Language
import Type.Proxy(Proxy(..))
import Data.DateTime(DateTime)
import Data.Date(Date)
import Data.Maybe (Maybe)

type Experiments = (
      id :: Auto Int,
      code :: Int,
      name :: String,
      description :: String,
      added :: Default DateTime
)

experiments :: Table "experiments" Experiments
experiments = Table

_description :: Proxy "description"
_description = Proxy

_added :: Proxy "added"
_added = Proxy

_code :: Proxy "code"
_code = Proxy
