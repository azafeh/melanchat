module Server.Leaderboard.Database where

import Prelude
import Server.Types
import Shared.Leaderboard.Types
import Droplet.Language
import Server.Database as SD
import Server.Database.KarmaLeaderboard
import Shared.Unsafe as SU

fetchTop10 :: ServerEffect (Array LeaderboardUser)
fetchTop10 = SD.unsafeQuery "select u.name, u.avatar, position, current_karma karma from karma_leaderboard k join users u on k.ranker = u.id order by k.id limit 10" {}

userPosition :: Int -> ServerEffect Int
userPosition loggedUserID = _.position <<< SU.fromJust <$> (SD.single $ select _position # from karma_leaderboard # wher (_ranker .=. loggedUserID))

fetchInBetween10 :: Int -> ServerEffect (Array LeaderboardUser)
fetchInBetween10 position = SD.unsafeQuery "select u.name, u.avatar, position, current_karma karma from karma_leaderboard k join users u on k.ranker = u.id where position between greatest(1, @position - 5) and @position + 5 order by position" { position }