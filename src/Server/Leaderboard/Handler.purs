module  Server.Leaderboard.Handler where

import Prelude
import Server.Types
import Shared.Types

import Run as R
import Server.Leaderboard.Database as SLD
import Server.Leaderboard.Template as SLT
import Shared.Newtype as SN

leaderboard :: { guards :: { loggedUserID :: Int } } -> ServerEffect String
leaderboard { guards: { loggedUserID } } = do
      top10 <- SLD.fetchTop10
      userPosition <- SLD.userPosition loggedUserID
      inBetween10 <- SLD.fetchInBetween10 userPosition
      R.liftEffect $ SLT.template { top10, inBetween10, userPosition }

