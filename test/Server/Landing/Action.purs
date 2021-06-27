module Test.Server.Landing.Action where

import Prelude
import Shared.Types
import Server.Types
import Data.Maybe (Maybe(..))

import Run as R
import Server.AccountValidation (emailAlreadyRegisteredMessage, invalidEmailMessage, invalidPasswordMessage)
import Server.Database as SD
import Server.Database.Users as SDU
import Server.Landing.Action as SLA
import Server.Landing.Database as SLD
import Server.Database.KarmaHistories
import Server.Database.KarmaLeaderboard
import Server.Database.Suggestions
import Data.BigInt as BG
import Server.Token as ST
import Shared.Unsafe as SU
import Test.Server as TS

import Test.Server.User (email, password)
import Test.Server.User as STU
import Droplet.Language
import Test.Unit (TestSuite)
import Test.Unit as TU
import Server.Database.Fields
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "landing actions" do
            let registerExceptionTest rl = do
                  void $ SLA.register rl
                  users <- STU.userCount
                  R.liftAff $ TUA.equal 0 users

            TU.test "register does not accept empty fields" $
                  TS.serverActionCatch (TS.catch invalidEmailMessage) $ registerExceptionTest {
                        email: "",
                        password: "",
                        captchaResponse: Nothing
                  }

            TU.test "register does not accept empty password" $
                  TS.serverActionCatch (TS.catch invalidPasswordMessage) $ registerExceptionTest {
                        email,
                        password: "",
                        captchaResponse: Nothing
                  }

            TU.test "register does not accept existing email" $
                  TS.serverActionCatch (TS.catch emailAlreadyRegisteredMessage) $ do
                        void $ SLD.createUser {
                              email,
                              name: "sdsd",
                              password,
                              headline: "sd",
                              description: "ss"
                        }
                        registerExceptionTest $ {
                              email,
                              password,
                              captchaResponse: Nothing
                        }

            TU.test "register creates user" $
                  TS.serverAction $ do
                        _ <- SLA.register $ {
                              email,
                              password,
                              captchaResponse: Nothing
                        }
                        maybeUser <- SDU.userBy (Email email)
                        case maybeUser of
                              Nothing -> R.liftAff $ TU.failure "user not created!"
                              Just user -> do
                                    hashed <- ST.hashPassword password
                                    R.liftAff $ TUA.equal hashed user.password

            TU.test "register creates karma" $
                  TS.serverAction $ do
                        void $ SLA.register {
                              email,
                              password,
                              captchaResponse: Nothing
                        }
                        { id } <- SU.fromJust <$> (SDU.userBy $ Email email)
                        history <- SD.single $ select (count _id # as c) # from karma_histories # wher (_target .=. id)
                        R.liftAff $ TUA.equal (Just {c: BG.fromInt 1}) history
                        leaderboard <- SD.single $ select (count _id # as c) # from karma_leaderboard # wher (_ranker .=. id)
                        R.liftAff $ TUA.equal (Just {c: BG.fromInt 1}) leaderboard

            TU.test "register creates suggestion" $
                  TS.serverAction $ do
                        void $ SLA.register {
                              email,
                              password,
                              captchaResponse: Nothing
                        }
                        { id } <- SU.fromJust <$> (SDU.userBy $ Email email)
                        suggestion <- SD.single $ select (count _id # as c) # from suggestions # wher (_suggested .=. id)
                        R.liftAff $ TUA.equal (Just {c: BG.fromInt 1}) suggestion






