module Server.Database.KarmaLeaderboard where

import Droplet.Language
import Type.Proxy (Proxy(..))
import Data.DateTime(DateTime)

type KarmaLeaderboard = (
      id :: Auto Int,
      ranker :: Int,
      position :: Int,
      current_karma :: Int,
      gained :: Int,
      date :: Default DateTime
)

karma_leaderboard :: Table "karma_leaderboard" KarmaLeaderboard
karma_leaderboard = Table



_ranker :: Proxy "ranker"
_ranker = Proxy

_position :: Proxy "position"
_position = Proxy

_current_karma :: Proxy "current_karma"
_current_karma = Proxy

_gained :: Proxy "gained"
_gained = Proxy

_date :: Proxy "date"
_date = Proxy

