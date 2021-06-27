module Server.Leaderboard.Template where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Flame.Html.Attribute as HA
import Shared.Leaderboard.Types
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (defaultParameters)
import Shared.Leaderboard.View as SLV
import Server.Template as ST
import Shared.Avatar as SA

template :: { top10 :: Array LeaderboardUser, userPosition :: Int, inBetween10 :: Array LeaderboardUser } -> Effect String
template { top10, inBetween10, userPosition } =
      F.preMount (QuerySelector ".karma-leaderboard") {
            view: SLV.view,
            init: {
                  toggleBoard: InBetween10,
                  top10,
                  inBetween10,
                  userPosition
            }
      }

