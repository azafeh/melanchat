module Test.Server.User where

import Prelude
import Droplet.Language
import Server.Database as SD
import Server.Database.Users
import Data.Maybe(Maybe(..))
import Shared.Unsafe as SU
import Server.Types
import Data.BigInt as DB
import Server.Database.Fields

userCount :: ServerEffect Int
userCount = do
    count <- SD.single $ select (count _id # as c) # from users
    pure $ case count of
        Just { c } -> SU.fromJust $ DB.toInt c
        Nothing -> 0

email :: String
email = "e@a.com"

password :: String
password = "hunter12"