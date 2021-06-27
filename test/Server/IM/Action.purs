module Test.Server.IM.Action where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.String.Regex as DSR
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe as DSRU
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))

import Debug (spy)
import Run as R
import Server.Database as SD
import Server.File (imageTooBigMessage, invalidImageMessage)
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.Landing.Database as SLD
import Shared.Options.File (maxImageSize)
import Shared.Unsafe ((!@))
import Test.Server as TS
import Test.Server.Model (baseUser)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "im actions" do
            TU.test "suggest filters out blocked users" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        suggestions <- SIA.suggest userID 0 Nothing
                        R.liftAff <<< TUA.equal 1 $ DA.length suggestions

                        void $ SIA.blockUser userID anotherUserID
                        suggestions' <- SIA.suggest userID 0 Nothing
                        R.liftAff <<< TUA.equal 0 $ DA.length suggestions'

            TU.test "suggest includes all users if impersonating" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        Tuple id _ <- SIA.processMessage userID anotherUserID 2 $ Text "oi"
                        suggestions <- SIA.suggest userID 0 <<< Just $ ArrayPrimaryKey []
                        R.liftAff <<< TUA.equal 1 $ DA.length suggestions

            TU.test "suggest avoids given users if impersonating" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        suggestions <- SIA.suggest userID 0 <<< Just $ ArrayPrimaryKey [anotherUserID]
                        R.liftAff <<< TUA.equal 0 $ DA.length suggestions

            TU.test "report also blocks user" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        void $ SIA.reportUser userID { userID: anotherUserID, reason: Spam, comment: Nothing }
                        count <- SD.single $ select (count _id # as c) # from blocks # wher (_blocker .=. userID .&&. _blocked .=. anotherUserID)
                        R.liftAff $ TUA.equal (Just {c: 1}) count

            TU.test "processMessage creates history" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        Tuple id _ <- SIA.processMessage userID anotherUserID 2 $ Text "oi"
                        R.liftAff $ TUA.equal userID id
                        count <- SD.single $ select (count _id # as c) # from histories # wher (_sender .=. userID .&&. _recipient .=. anotherUserID)
                        R.liftAff $ TUA.equal (Just {c: 1}) count

            TU.test "processMessage sets chat starter" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        Tuple id _ <- SIA.processMessage userID anotherUserID 2 $ Text "oi"
                        R.liftAff $ TUA.equal userID id
                        chatStarter <- SD.single $ select _sender # from histories # orderBy _id # limit 1
                        R.liftAff $ TUA.equal userID chatStarter

            TU.test "processMessage accepts files" $
                  TS.serverActionCatch (TS.catch invalidImageMessage) $ do
                        Tuple userID anotherUserID <- setUpUsers
                        Tuple _ message <- SIA.processMessage userID anotherUserID 2 $ Image "hey" "data:image/png;base64,ya"
                        R.liftAff <<< TUA.assert "returns file" $ DSR.test (DSRU.unsafeRegex "!\\[hey\\]((.*)/upload/(.*).png)" noFlags) message

            TU.test "processMessage sanitizes input" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        Tuple _ message <- SIA.processMessage userID anotherUserID 2 $ Text "<script><script>"
                        R.liftAff $ TUA.equal "" message

            TU.test "processMessage does not accept files too large" $
                  TS.serverActionCatch (TS.catch imageTooBigMessage) $ do
                        Tuple userID anotherUserID <- setUpUsers
                        SIA.processMessage userID anotherUserID 2 <<< Image "hey" $ "data:image/png;base64," <> (DS.joinWith "" $ DA.replicate (maxImageSize * 10) "a")

            TU.test "listContacts orders contacts by descreasing date" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        yetAnotherUserID <- SLD.createUser $ baseUser { email = "d@d.com"}
                        void <<< SIA.processMessage userID yetAnotherUserID 2 $ Text "ola"
                        void <<< SIA.processMessage userID anotherUserID 1 $ Text "oi"
                        void <<< SIA.processMessage userID yetAnotherUserID 2 $ Text "hey"
                        contacts <- SIA.listContacts userID 0
                        R.liftAff <<< TUA.equal 2 $ DA.length contacts
                        R.liftAff $ TUA.equal yetAnotherUserID (contacts !@ 0).user.id
                        R.liftAff $ TUA.equal anotherUserID (contacts !@ 1).user.id

            TU.test "listContacts matches contact and (first message) chat history" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        yetAnotherUserID <- SLD.createUser $ baseUser { email = "d@d.com"}
                        void <<< SIA.processMessage userID anotherUserID 1 $ Text "oi"
                        void <<< SIA.processMessage userID yetAnotherUserID 2 $ Text "ola"
                        void <<< SIA.processMessage userID yetAnotherUserID 3 $ Text "hey"
                        contacts <- SIA.listContacts userID 0
                        R.liftAff <<< TUA.equal 2 $ DA.length contacts
                        R.liftAff $ TUA.equal yetAnotherUserID (contacts !@ 0).user.id
                        R.liftAff <<< TUA.equal 1 $ DA.length (contacts !@ 0).history
                        R.liftAff $ TUA.equal anotherUserID (contacts !@ 1).user.id
                        R.liftAff <<< TUA.equal 1 $ DA.length (contacts !@ 1).history

            TU.test "listMissedEvents finds missed contacts" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        yetAnotherUserID <- SLD.createUser $ baseUser { email = "d@d.com"}
                        Tuple id _ <- SIA.processMessage anotherUserID userID 1 $ Text "oi"
                        void <<< SIA.processMessage yetAnotherUserID userID 2 $ Text "ola"
                        void <<< SIA.processMessage yetAnotherUserID userID 3 $ Text "hey"
                        { contacts } <- SIA.listMissedEvents userID Nothing (Just $ id - 1)
                        R.liftAff <<< TUA.equal 2 $ DA.length contacts

                        R.liftAff $ TUA.equal anotherUserID (contacts !@ 0).user.id
                        R.liftAff <<< TUA.equal 1 $ DA.length (contacts !@ 0).history

                        R.liftAff $ TUA.equal yetAnotherUserID (contacts !@ 1).user.id
                        R.liftAff <<< TUA.equal 2 $ DA.length (contacts !@ 1).history

            TU.test "listMissedEvents ignores delivered messages" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        yetAnotherUserID <- SLD.createUser $ baseUser { email = "d@d.com"}
                        Tuple id _ <- SIA.processMessage anotherUserID userID 1 $ Text "oi"
                        Tuple anotherID _ <- SIA.processMessage yetAnotherUserID userID 2 $ Text "ola"
                        void <<< SIA.processMessage yetAnotherUserID userID 3 $ Text "hey"
                        SID.changeStatus userID Delivered [id, anotherID]
                        { contacts } <- SIA.listMissedEvents userID Nothing (Just $ id - 1)
                        R.liftAff <<< TUA.equal 1 $ DA.length contacts
                        R.liftAff $ TUA.equal yetAnotherUserID (contacts !@ 0).user.id
                        R.liftAff <<< TUA.equal 1 $ DA.length (contacts !@ 0).history

            TU.test "listMissedEvents finds temporary ids" $
                  TS.serverAction $ do
                        Tuple userID anotherUserID <- setUpUsers
                        yetAnotherUserID <- SLD.createUser $ baseUser { email = "d@d.com"}
                        void <<< SIA.processMessage userID anotherUserID 1 $ Text "oi"
                        void <<< SIA.processMessage userID yetAnotherUserID 2 $ Text "ola"
                        void <<< SIA.processMessage userID yetAnotherUserID 3 $ Text "hey"
                        { messageIDs } <- SIA.listMissedEvents userID (Just 0) Nothing
                        R.liftAff <<< TUA.equal [1, 2, 3] $ map _.temporaryID messageIDs

      where setUpUsers = do
                  userID <- SLD.createUser $ baseUser { email = "b@b.com" }
                  anotherUserID <- SLD.createUser $ baseUser { email = "c@c.com" }
                  pure $ Tuple userID anotherUserID