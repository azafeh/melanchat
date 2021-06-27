module Test.Server.Profile.Action where

import Prelude

import Data.Maybe (Maybe(..))

import Run as R
import Server.Database as SD
import Server.Landing.Database as SLD
import Server.Profile.Action as SPA
import Shared.Options.File (imageBasePath)
import Test.Server as TS
import Test.Server.Model (baseUser)
import Test.Unit (TestSuite)
import Droplet.Language
import Server.Database.Fields
import Server.Database.Users
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "landing actions" do
            TU.test "saveProfile ignores current picture" $
                  TS.serverAction $ do
                        userID <- SLD.createUser $ baseUser { email = "b@b.com" }
                        void $ SPA.saveProfile userID { id: userID, karma:0, karmaPosition:0, gender: Nothing, country: Nothing, name:"a", age: Nothing, headline:"a", description:"a", avatar: Just $ imageBasePath <> "upload/avatar.png", languages:[], tags:[] }
                        avatar <- SD.single $ select _avatar # from users # wher (_id .=. userID)
                        R.liftAff $ TUA.equal (Just { avatar: Just "avatar.png" }) avatar

