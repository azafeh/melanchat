module Server.Database.KarmaHistories where

import Droplet.Language
import Type.Proxy (Proxy(..))
import Data.DateTime(DateTime)

type KarmaHistories = (
      id :: Auto Int,
      target :: Int,
      amount :: Int,
      date :: Default DateTime
)

karma_histories :: Table "karma_histories" KarmaHistories
karma_histories = Table



_target :: Proxy "target"
_target = Proxy

_amount :: Proxy "amount"
_amount = Proxy

_date :: Proxy "date"
_date = Proxy