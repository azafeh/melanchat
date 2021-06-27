module Server.Settings.Database where

import Prelude

import Data.Tuple.Nested ((/\))

import Droplet.Language
import Server.Database.Users
import Server.Database as SD
import Server.Types (ServerEffect)
import Server.Database.Fields

changeEmail :: Int -> String -> ServerEffect Unit
changeEmail loggedUserID email = SD.execute $ update users # set (_email /\ email) # wher (_id .=. loggedUserID)

changePassword :: Int -> String -> ServerEffect Unit
changePassword loggedUserID password = SD.execute $ update users # set (_password /\ password) # wher (_id .=. loggedUserID)

terminateAccount :: Int -> ServerEffect Unit
terminateAccount loggedUserID = SD.execute $ delete # from users # wher (_id .=. loggedUserID) --cascades


