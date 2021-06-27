module Client.IM.Contacts where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages)
import Client.IM.Flame as CIF
import Client.IM.Notification as CIU
import Client.IM.Notification as CIUN
import Client.IM.Scroll as CIS
import Client.IM.WebSocket as CIW
import Data.Array ((!!), (..))
import Data.Array as DA
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect.Class (liftEffect)
import Shared.Experiments.Types
import Flame ((:>))
import Flame as F
import Shared.IM.Types
import Shared.IM.Contact as SIC
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.HTML.HTMLElement as WHH
import Web.Socket.WebSocket (WebSocket)

resumeChat :: Int -> Maybe Int -> IMModel -> MoreMessages
resumeChat searchID impersonating model@{ contacts, chatting, smallScreen } =
      let   index = DA.findIndex (\cnt -> cnt.user.id == searchID && cnt.impersonating == impersonating) contacts
            cnt@{ shouldFetchChatHistory, user: { id } } = SIC.chattingContact contacts index
      in
            if index == chatting then
                  F.noMessages model
             else
                  model {
                        chatting = index,
                        fullContactProfileVisible = false,
                        toggleChatModal = HideChatModal,
                        initialScreen = false,
                        selectedImage = Nothing,
                        failedRequests = []
                  } :> ([
                        CIF.next UpdateReadCount,
                        CIS.scrollLastMessage',
                        CIF.next <<< SpecialRequest $ FetchHistory shouldFetchChatHistory
                  ] <> smallScreenEffect)
      where smallScreenEffect = if smallScreen then [] else [CIF.next $ FocusInput ChatInput]

markRead :: WebSocket -> IMModel -> MoreMessages
markRead webSocket =
      case _ of
            model@{
                  user: { id: userID },
                  contacts,
                  chatting: Just index
            } -> updateStatus model {
                  newStatus: Read,
                  index,
                  webSocket,
                  sessionUserID: userID,
                  contacts
            }
            model -> F.noMessages model

updateStatus :: IMModel -> {
      sessionUserID :: Int,
      webSocket :: WebSocket,
      contacts :: Array Contact,
      index :: Int,
      newStatus :: MessageStatus
} -> MoreMessages
updateStatus model@{ experimenting } { webSocket, index, sessionUserID, contacts, newStatus } =
      let contactRead@{ history, user: { id: contactUserID } } = contacts !@ index
          messagesRead = DA.mapMaybe toChange history
      in
            if DA.null messagesRead then
                  F.noMessages model
             else
                  let updatedModel@{ contacts } = updateContacts contactRead
                  in CIF.nothingNext updatedModel $ liftEffect do
                        changeStatus contactUserID messagesRead
                        alertUnread contacts

      where toChange { recipient, id, status }
                  | status >= Sent && status < newStatus && recipient == sessionUserID = Just id
                  | otherwise = Nothing

            read historyEntry@( { recipient, id, status })
                  | status >= Sent && status < newStatus && recipient == sessionUserID = historyEntry { status = newStatus }
                  | otherwise = historyEntry

            updateContacts contactRead@{ history } = model {
                  contacts = SU.fromJust $  DA.updateAt index (contactRead { history = map read history }) contacts
            }

            changeStatus contactUserID messages = CIW.sendPayload webSocket $ ChangeStatus {
                  userID: contactUserID,
                  status: newStatus,
                  ids: messages,
                  persisting: case experimenting of
                        Just (Impersonation (Just _)) -> false
                        _ -> true
            }

            alertUnread contacts = CIUN.updateTabCount sessionUserID contacts

checkFetchContacts :: IMModel -> MoreMessages
checkFetchContacts model@{ contacts, freeToFetchContactList }
      | freeToFetchContactList = model :> [ Just <<< SpecialRequest <<< FetchContacts <$> getScrollBottom ]

      where getScrollBottom = liftEffect do
                  element <- CCD.unsafeGetElementByID ContactList
                  top <- WDE.scrollTop element
                  height <- WDE.scrollHeight element
                  offset <- WHH.offsetHeight <<< SU.fromJust $ WHH.fromElement element
                  pure $ top == height - offset

      | otherwise = F.noMessages model

fetchContacts :: Boolean -> IMModel -> MoreMessages
fetchContacts shouldFetch model@{ contacts, freeToFetchContactList, experimenting }
      | shouldFetch = model {
            freeToFetchContactList = false
      } :> if DM.isJust experimenting then [] else [CCN.retryableResponse (FetchContacts true) DisplayContacts $ request.im.contacts { query: { skip: DA.length contacts }}]
      | otherwise = F.noMessages model

--paginated contacts
displayContacts :: Array Contact -> IMModel -> MoreMessages
displayContacts newContacts model@{ contacts } = updateDisplayContacts newContacts [] model

--new chats
displayNewContacts :: Array Contact -> IMModel -> MoreMessages
displayNewContacts newContacts model@{ contacts } = updateDisplayContacts newContacts (map (\cnt -> Tuple cnt.user.id cnt.impersonating) newContacts) model

--new chats from impersonation experiment
displayImpersonatedContacts :: Int -> HistoryMessage -> Array Contact -> IMModel -> MoreMessages
displayImpersonatedContacts id history newContacts = displayNewContacts (map (_ { shouldFetchChatHistory = false, impersonating = Just id, history = [history] }) newContacts)

resumeMissedEvents :: MissedEvents -> IMModel -> MoreMessages
resumeMissedEvents { contacts: missedContacts, messageIDs } model@{ contacts, user: { id: senderID } } =
      let missedFromExistingContacts = map markSenderError $ DA.updateAtIndices (map getExisting existing) contacts
          missedFromNewContacts = map getNew new
      in CIU.notifyUnreadChats (model {
            --wew lass
            contacts = missedFromNewContacts <> missedFromExistingContacts
      }) $ map (\cnt -> Tuple cnt.user.id cnt.impersonating) missedContacts
      where messageMap = DH.fromArrayBy _.temporaryID _.id messageIDs
            markSenderError contact@{ history } = contact {
                  history = map updateSenderError history
            }
            updateSenderError history@{ sender, status, id }
                  | status == Sent && sender == senderID =
                        if DH.member id messageMap then  --received or not by the server
                              history {
                                    status = Received,
                                    id = SU.lookup id messageMap
                              }
                        else
                              history { status = Errored }
                  | otherwise = history

            indexesToIndexes = DA.zip (0 .. DA.length missedContacts) $ findContact <$> missedContacts
            existing = DA.filter (DM.isJust <<< DT.snd) indexesToIndexes
            new = DA.filter (DM.isNothing <<< DT.snd) indexesToIndexes

            getNew (Tuple newIndex _) = missedContacts !@ newIndex

            getExisting (Tuple existingIndex contactsIndex) = SU.fromJust do
                  index <- contactsIndex
                  currentContact <- contacts !! index
                  contact <- missedContacts !! existingIndex
                  pure <<< Tuple index $ currentContact {
                        history = currentContact.history <> contact.history
                  }

            findContact {user: { id }} = DA.findIndex (sameContact id) contacts
            sameContact userID {user: { id }} = userID == id

updateDisplayContacts :: Array Contact -> Array (Tuple Int (Maybe Int)) -> IMModel -> MoreMessages
updateDisplayContacts newContacts userIDs model@{ contacts } =
      CIU.notifyUnreadChats (model {
            contacts = contacts <> newContacts,
            freeToFetchContactList = true
      }) userIDs