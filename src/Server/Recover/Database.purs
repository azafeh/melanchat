module Server.Recover.Database where

import Prelude
import Server.Types
import Shared.Types
import Droplet.Language
import Server.Database.Users
import Server.Database.Recoveries
import Server.Database.Fields
import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))

import Server.Database as SD

--REFACTOR: consider making the uuid token the primary key
insertRecover :: Int -> String -> ServerEffect Unit
insertRecover id token = void <<< SD.execute $ insert # into recoveries (_uuid /\ _recoverer) # values (token /\ id)

selectRecoverer :: String -> ServerEffect (Maybe Int)
selectRecoverer token = do
      result <- SD.unsafeSingle "select recoverer from recoveries where uuid = @token and active = true and created >= (now() at time zone 'utc') - interval '1 day'" {token} :: BaseEffect _ (Maybe {recoverer :: Int})
      pure (_.recoverer <$> result)

recoverPassword :: String -> Int -> String -> ServerEffect Unit
recoverPassword token id password = SD.withTransaction $ \connection -> do
      SD.executeWith connection $ update recoveries # set (_active /\ false) # wher (_uuid .=. token)
      SD.executeWith connection $ update users # set (_password /\ password) # wher (_id .=. id)