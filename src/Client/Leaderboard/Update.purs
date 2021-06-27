module Client.Leaderboard.Update where

import Prelude
import Shared.Types
import Shared.Leaderboard.Types
import Flame as F
import Flame (ListUpdate)

update :: ListUpdate LeaderboardModel LeaderboardMessage
update model =
      case _ of
            ToggleBoardDisplay toggle -> F.noMessages $ model {
                toggleBoard = toggle
            }