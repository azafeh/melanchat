module Test.Client.IM.Contacts where

import Prelude
import Shared.Types

import Client.IM.Contacts as CICN
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.Experiments.Impersonation (batman)
import Shared.Unsafe ((!@))
import Test.Client.Model (contact, historyMessage, imUser, imUserID, model, webSocket)
import Test.Unit (TestSuite)
import Shared.IM.Types
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "im contacts update" do
            TU.test "resumeChat sets chatting" do
                  let m@{ chatting } = DT.fst <<< CICN.resumeChat anotherContactID Nothing $ model {
                        chatting = Nothing,
                        contacts = [contact { user = imUser }, anotherContact]
                  }
                  TUA.equal (Just 1) chatting

                  let { chatting } = DT.fst $ CICN.resumeChat imUserID Nothing m
                  TUA.equal (Just 0) chatting

            TU.test "resumeChat finds impersonation contact" do
                  let m@{ chatting } = DT.fst <<< CICN.resumeChat imUser.id (Just batman.id) $ model {
                        chatting = Nothing,
                        contacts = [contact { user = imUser }, anotherContact { user = imUser, impersonating = Just batman.id}]
                  }
                  TUA.equal (Just 1) chatting

            TU.test "updateStatus sets recieved messages as read" do
                  let { contacts } = DT.fst <<< CICN.markRead webSocket $ model {
                        chatting = Just 1,
                        contacts = [contact, anotherContact]
                  }
                  TUA.equal [Tuple 1 Read, Tuple 2 Received, Tuple 3 Read] <<< map (\( { id, status}) -> Tuple id status) $ (contacts !@ 1).history

            TU.test "displayContacts shows next page" do
                  let { contacts } = DT.fst <<< CICN.displayContacts [contact] $ model {
                        contacts = []
                  }
                  TUA.equal [contact] contacts

            TU.test "resumeMissedEvents adds missed contacts" do
                  let { contacts } = DT.fst <<< CICN.resumeMissedEvents { contacts: [contact], messageIDs:[] } $ model {
                        contacts = []
                  }
                  TUA.equal [contact] contacts

            TU.test "resumeMissedEvents adds missed messages" do
                  let   updated = [contact { history = [historyMessage]}]
                        { contacts } = DT.fst <<< CICN.resumeMissedEvents { contacts: updated, messageIDs:[] } $ model {
                              contacts = [contact { history = [] }]
                        }
                  TUA.equal updated contacts

            TU.test "resumeMissedEvents sets sucessfull messages" do
                  let   temporaryID = 1
                        newID = 10
                        existing = [contact {
                              history = [historyMessage {
                                    status = Sent,
                                    id = temporaryID
                              }]
                        }]
                        { contacts } = DT.fst <<< CICN.resumeMissedEvents { contacts: [], messageIDs: [{ id: newID, temporaryID}] } $ model {
                              contacts = existing
                        }
                        updated = [contact {
                              history = [historyMessage {
                                    status = Received,
                                    id = newID
                              }]
                        }]
                  TUA.equal updated contacts

            TU.test "resumeMissedEvents sets errored messages" do
                  let   temporaryID = 1
                        existing = [contact {
                              history = [historyMessage {
                                    status = Sent,
                                    id = temporaryID
                              }]
                        }]
                        { contacts } = DT.fst <<< CICN.resumeMissedEvents { contacts: [], messageIDs: [] } $ model {
                              contacts = existing
                        }
                        updated = [contact {
                              history = [historyMessage {
                                    status = Errored
                              }]
                        }]
                  TUA.equal updated contacts

      where anotherContactID = imUser.id + 1
            anotherContact = contact {
                  user = imUser { id = anotherContactID },
                  history = [{
                        id: 1,
                        status: Received,
                        sender: 32,
                        recipient: imUser.id,
                        content: "1",
                        date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
                  }, {
                        id:  2,
                        status: Received,
                        sender: imUser.id,
                        recipient:  32,
                        content: "2",
                        date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
                  },{
                        id:  3,
                        status: Received,
                        sender:  32,
                        recipient: imUser.id,
                        content: "3",
                        date: EU.unsafePerformEffect $ map DateTimeWrapper EN.nowDateTime
                  }]
            }
