module Client.IM.Main where
--refactor: this file needs to be broken down into modules

import Prelude
import Shared.Types

import Client.Common.DOM (setChatExperiment)
import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Common.Network as CCNT
import Client.Common.Types (CurrentWebSocket)
import Client.IM.Chat as CIC
import Client.IM.Contacts as CICN
import Client.IM.Flame (NextMessage, NoMessages, MoreMessages)
import Client.IM.Flame as CIF
import Client.IM.History as CIH
import Client.IM.Notification as CIUC
import Client.IM.Scroll as CISM
import Client.IM.Suggestion as CIS
import Client.IM.UserMenu as CIU
import Client.IM.WebSocket (WebSocket, onClose, onMessage, onOpen)
import Client.IM.WebSocket as CIW
import Control.Monad.Except as CME
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM

import Data.Symbol as TDS
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random as ERD
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Timer as ET
import Effect.Unsafe as EU
import Flame (ListUpdate, QuerySelector(..), (:>))
import Flame as F
import Flame.Subscription as FE
import Flame.Subscription as FS
import Flame.Subscription.Document as FSD
import Flame.Subscription.Unsafe.CustomEvent as FSUC
import Flame.Subscription.Window as FSW
import Foreign as FO
import Shared.Breakpoint (mobileBreakpoint)
import Shared.IM.View as SIV
import Shared.JSON as SJ
import Shared.Options.MountPoint (imID, profileID)
import Shared.Routes (routes)
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.DOM.Node as WDN
import Web.Event.Event as WEE
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.File.FileReader as WFR
import Web.HTML as WH
import Web.HTML.Event.EventTypes (focus)
import Web.HTML.Event.PopStateEvent.EventTypes (popstate)
import Web.HTML.HTMLElement as WHHE
import Web.HTML.Window as WHW

main :: Effect Unit
main = do
      webSocket <- CIW.createWebSocket
      --web socket needs to be a ref as any time the connection can be closed and recreated by events
      webSocketRef <- ER.new { webSocket, ponged : true }
      fileReader <- WFR.fileReader
      F.resumeMount (QuerySelector ".im") imID {
            view: SIV.view true,
            subscribe: [
                  --display settings/profile/etc page menus
                  FSD.onClick' ToggleUserContextMenu,
                  --focus event has to be on the window as chrome is a whiny baby about document
                  FSW.onFocus UpdateReadCount,
                  --events from chat experiment
                  FS.onCustomEvent setChatExperiment SetChatExperiment
            ],
            init: [],
            update: update { fileReader, webSocketRef }
      }
      setUpWebSocket webSocketRef
      width <- CCD.screenWidth
      let smallScreen = width < mobileBreakpoint
      --keep track of mobile (-like) screens for things that cant be done with media queries
      when smallScreen $ FS.send imID SetSmallScreen
      --disable the back button on desktop/make the back button go back to previous screen on mobile
      CCD.pushState $ routes.im.get {}
      historyChange smallScreen
      --for drag and drop
      CCF.setUpBase64Reader fileReader (SetSelectedImage <<< Just) imID
      --image upload
      input <- CCD.unsafeGetElementByID ImageFileInput
      CCF.setUpFileChange (SetSelectedImage <<< Just) input imID
      --notification permission (desktop)
      unless smallScreen checkNotifications

update :: _ -> ListUpdate IMModel IMMessage
update { webSocketRef, fileReader } model =
      case _ of
            --chat
            InsertLink -> CIC.insertLink model
            ToggleChatModal modal -> CIC.toggleModal modal model
            DropFile event -> CIC.catchFile fileReader event model
            EnterBeforeSendMessage event -> CIC.enterBeforeSendMessage event model
            ForceBeforeSendMessage -> CIC.forceBeforeSendMessage model
            ResizeChatInput event -> CIC.resizeChatInput event model
            BeforeSendMessage content -> CIC.beforeSendMessage content model
            SendMessage content date -> CIC.sendMessage webSocket content date model
            SetMessageContent cursor content -> CIC.setMessage cursor content model
            Apply markup -> CIC.applyMarkup markup model
            SetSelectedImage maybeBase64 -> CIC.setSelectedImage maybeBase64 model
            SetSmallScreen -> setSmallScreen model
            SetEmoji event -> CIC.setEmoji event  model
            ToggleMessageEnter -> CIC.toggleMessageEnter model
            FocusInput elementID -> focusInput elementID model
            --contacts
            ResumeChat (Tuple id impersonating) -> CICN.resumeChat id impersonating model
            UpdateReadCount -> CICN.markRead webSocket model
            CheckFetchContacts -> CICN.checkFetchContacts model
            SpecialRequest (FetchContacts shouldFetch) -> CICN.fetchContacts shouldFetch model
            DisplayContacts contacts -> CICN.displayContacts contacts model
            DisplayNewContacts contacts -> CICN.displayNewContacts contacts model
            DisplayImpersonatedContact id history contacts -> CICN.displayImpersonatedContacts id history contacts model
            ResumeMissedEvents missed -> CICN.resumeMissedEvents missed model
            --history
            CheckFetchHistory -> CIH.checkFetchHistory model
            SpecialRequest (FetchHistory shouldFetch) -> CIH.fetchHistory shouldFetch model
            DisplayHistory history -> CIH.displayHistory history model
            --suggestion
            FetchMoreSuggestions -> CIS.fetchMoreSuggestions model
            ResumeSuggesting -> CIS.resumeSuggesting model
            ToggleContactProfile -> CIS.toggleContactProfile model
            SpecialRequest PreviousSuggestion -> CIS.previousSuggestion model
            SpecialRequest NextSuggestion -> CIS.nextSuggestion model
            SpecialRequest (BlockUser id) -> CIS.blockUser webSocket id model
            DisplayMoreSuggestions suggestions -> CIS.displayMoreSuggestions suggestions model
            --user menu
            ToggleInitialScreen toggle -> CIU.toggleInitialScreen toggle model
            Logout -> CIU.logout model
            ToggleUserContextMenu event -> toggleUserContextMenu event model
            SpecialRequest (ToggleModal toggle) -> CIU.toggleModal toggle model
            SetModalContents file root html -> CIU.setModalContents file root html model
            SetContextMenuToggle toggle -> CIU.toogleUserContextMenu toggle model
            --main
            AskChatExperiment -> askExperiment model
            SetChatExperiment experiment -> setExperiment experiment model
            ReloadPage -> reloadPage model
            ReceiveMessage payload isFocused -> receiveMessage webSocket isFocused payload model
            SetNameFromProfile name -> setName name model
            AskNotification -> askNotification model
            ToggleAskNotification -> toggleAskNotification model
            PreventStop event -> preventStop event model
            ToggleConnected isConnected -> toggleConnectedWebSocket isConnected model
            SpecialRequest CheckMissedEvents -> checkMissedEvents model
            SetField setter -> F.noMessages $ setter model
            ToggleFortune isVisible -> toggleFortune isVisible model
            DisplayFortune sequence -> displayFortune sequence model
            RequestFailed failure -> addFailure failure model
            SpecialRequest (ReportUser userID) -> report userID webSocket model
      where { webSocket } = EU.unsafePerformEffect $ ER.read webSocketRef -- u n s a f e

report :: Int -> WebSocket -> IMModel -> MoreMessages
report userID webSocket model@{ reportReason, reportComment } = case reportReason of
      Just rs -> CIS.updateAfterBlock userID (model {
            reportReason = Nothing,
            reportComment = Nothing
      }) :> [do
            result <- CCN.defaultResponse $ request.im.report { body: { userID, reason: rs, comment: reportComment } }
            case result of
                  Left _ -> pure <<< Just $ RequestFailed { request: ReportUser userID, errorMessage : "" }
                  _ -> do
                        liftEffect <<< CIW.sendPayload webSocket $ ToBlock { id: userID }
                        pure Nothing
      ]
      Nothing -> F.noMessages $ model {
            erroredFields = [ TDS.reflectSymbol (Proxy :: Proxy "reportReason") ]
      }

askExperiment :: IMModel -> MoreMessages
askExperiment model@{ experimenting } = model :> if DM.isNothing experimenting then [] else [ do
      liftEffect $ FSUC.broadcast setChatExperiment experimenting
      pure Nothing
]

setExperiment :: Maybe ExperimentData -> IMModel -> MoreMessages
setExperiment experiment model@{ toggleModal, contacts, experimenting, suggestionsPage } = model {
      chatting = Nothing,
      temporaryID = 3000000,
      contacts = if impersonating then [] else contacts,
      experimenting = experiment,
      toggleModal = if toggleModal == ShowExperiments then HideUserMenuModal else toggleModal,
      suggestionsPage = if impersonating then 0 else suggestionsPage
} :> if impersonating then [pure $ Just FetchMoreSuggestions] else []
      where
            impersonating = case experiment of
                  imp@(Just (Impersonation (Just _))) -> imp /= experimenting --avoid running more than once
                  _ -> false

reloadPage :: IMModel -> NextMessage
reloadPage model = CIF.nothingNext model $ liftEffect CCL.reload

askNotification :: IMModel -> MoreMessages
askNotification model = CIF.nothingNext (model { enableNotificationsVisible = false }) $ liftEffect CCD.requestNotificationPermission

--refactor: all messages like this can be dryed into a single function
toggleAskNotification :: IMModel -> NoMessages
toggleAskNotification model@{ enableNotificationsVisible } = F.noMessages $ model {
      enableNotificationsVisible = not enableNotificationsVisible
}

toggleUserContextMenu :: Event -> IMModel -> MoreMessages
toggleUserContextMenu event model@{ toggleContextMenu }
      | toggleContextMenu /= HideContextMenu =
            F.noMessages $ model { toggleContextMenu = HideContextMenu }
      | otherwise =
            model :> [
                  --we cant use node.contains as some of the elements are dynamically created/destroyed
                  liftEffect do
                  let element =  SU.fromJust $ do
                        target <- WEE.target event
                        WDE.fromEventTarget target
                  id <- WDE.id element
                  parent <- WDN.parentElement $ WDE.toNode element
                  parentID <- case parent of
                        Just e ->  WDE.id e
                        Nothing -> pure ""
                  pure <<< Just <<< SetContextMenuToggle $ toggle id parentID
            ]
      where toggle elementID parentID
                  | elementID == show UserContextMenu || parentID == show UserContextMenu = ShowUserContextMenu
                  | elementID == show SuggestionContextMenu || parentID == show SuggestionContextMenu = ShowSuggestionContextMenu
                  | elementID == show CompactProfileContextMenu || parentID == show CompactProfileContextMenu = ShowCompactProfileContextMenu
                  | elementID == show FullProfileContextMenu || parentID == show FullProfileContextMenu = ShowFullProfileContextMenu
                  | otherwise = HideContextMenu

focusInput :: ElementID -> IMModel -> NextMessage
focusInput elementID model = model :> [
      liftEffect do
            element <- CCD.getElementByID elementID
            WHHE.focus $ SU.fromJust do
                  e <- element
                  WHHE.fromElement e
            pure Nothing
]

addFailure :: RequestFailure -> IMModel -> NoMessages
addFailure failure@{ request } model@{ failedRequests, errorMessage } = F.noMessages $ model {
      failedRequests = failure : failedRequests,
      errorMessage = case request of
            BlockUser _ -> "Could not block user. Please try again"
            ReportUser _ -> "Could not report user. Please try again"
            PreviousSuggestion -> suggestionsError
            NextSuggestion -> suggestionsError
            _ -> errorMessage
}
      where suggestionsError = "Could not fetch suggestions. Please try again"

toggleFortune :: Boolean -> IMModel -> MoreMessages
toggleFortune isVisible model
      | isVisible = model :> [Just <<< DisplayFortune <$> CCNT.silentResponse (request.im.fortune {})]
      | otherwise = F.noMessages $ model {
            fortune = Nothing
      }

displayFortune :: String -> IMModel -> NoMessages
displayFortune sequence model = F.noMessages $ model {
      fortune = Just sequence
}

--refactor: this needs some serious cleanup
receiveMessage :: WebSocket -> Boolean -> WebSocketPayloadClient -> IMModel -> MoreMessages
receiveMessage webSocket isFocused wsPayload model@{
      user: { id: recipientID },
      contacts: currentContacts,
      suggestions,
      hash,
      blockedUsers
} = case wsPayload of
      CurrentHash newHash ->
            F.noMessages $ model {
                  imUpdated = newHash /= hash
            }
      ServerReceivedMessage { previousID, id, userID } ->
            F.noMessages $ model {
                  contacts = updateTemporaryID currentContacts userID previousID id
            }
      ServerChangedStatus { ids, status, userID } ->
            F.noMessages $ model {
                  contacts = updateStatus currentContacts userID ids status
            }
      BeenBlocked { id } ->
            F.noMessages <<< unsuggest id $ model { contacts = markContactUnavailable currentContacts id }
      NewIncomingMessage payload@{ id: messageID, userID, content: messageContent, date: messageDate, experimenting } ->
            --(for now) if the experiments dont match, discard the message
            if DA.elem userID blockedUsers || not (match userID experimenting)  then
                  F.noMessages model
            else let model' = unsuggest userID model in
                  case processIncomingMessage payload model' of
                  --this should also set status
                        Left userID ->
                              let message = case experimenting of
                                    Just (ImpersonationPayload { id: impersonationID }) ->
                                          DisplayImpersonatedContact impersonationID {
                                                status: Received,
                                                sender: userID,
                                                recipient: recipientID,
                                                id: messageID,
                                                content: messageContent,
                                                date: messageDate
                                          }
                                    _ -> DisplayNewContacts
                              in model' :> [CCNT.retryableResponse CheckMissedEvents message (request.im.singleContact { query: { id: userID }})]
                        --mark it as read if we received a message from the current chat
                        -- or as delivered otherwise
                        Right updatedModel@{
                              chatting: Just index,
                              contacts
                        } | isFocused && isChatting userID updatedModel ->
                              let Tuple furtherUpdatedModel messages = CICN.updateStatus updatedModel {
                                    sessionUserID: recipientID,
                                    contacts,
                                    newStatus: Read,
                                    webSocket,
                                    index
                              } in
                              furtherUpdatedModel :> (CISM.scrollLastMessage' : messages)
                        Right updatedModel@{
                              contacts
                        } ->
                              let   impersonationID = case experimenting of
                                          Just (ImpersonationPayload { id: impersonationID }) -> Just impersonationID
                                          _ -> Nothing
                                    Tuple furtherUpdatedModel messages = CICN.updateStatus updatedModel {
                                          index: SU.fromJust $ DA.findIndex (findContact userID impersonationID model.experimenting ) contacts,
                                          sessionUserID: recipientID,
                                          newStatus: Delivered,
                                          contacts,
                                          webSocket
                                    }
                              in
                                    furtherUpdatedModel :> (CIUC.notify' furtherUpdatedModel [Tuple payload.userID impersonationID] : messages)

      PayloadError payload -> case payload.origin of
            OutgoingMessage { id, userID } -> F.noMessages $ model {
                 contacts =
                        --assume that it is because the other user no longer exists
                        if payload.context == Just MissingForeignKey then
                              markContactUnavailable currentContacts userID
                         else
                              markErroredMessage currentContacts userID id
            }
            _ -> F.noMessages model
      where isChatting senderID { contacts, chatting } =
                  let { user: { id: recipientID }, impersonating } = contacts !@ SU.fromJust chatting
                  in recipientID == senderID

            match userID experimenting = case model.experimenting, experimenting of
                  Just (Impersonation (Just _)), Nothing -> false
                  Just (Impersonation (Just { id })), Just (ImpersonationPayload { id: otherID, sender }) -> id == otherID && not sender
                  Nothing, Just (ImpersonationPayload { sender }) -> sender
                  _, _ -> true

unsuggest :: Int -> IMModel -> IMModel
unsuggest userID model@{ suggestions, suggesting } = model {
      suggestions = DA.filter ((userID /= _) <<< _.id) suggestions,
      suggesting = (\i -> if i == 0 then 0 else i - 1) <$> suggesting
}

processIncomingMessage :: ClientMessagePayload -> IMModel -> Either Int IMModel
processIncomingMessage { id, userID, date, content, experimenting } model@{
      user: { id: recipientID },
      suggestions,
      contacts,
      chatting
} = case findAndUpdateContactList of
      Just contacts' ->
            Right $ model {
                  contacts = contacts'
            }
      Nothing -> Left userID
      where updateHistory { id, content, date } contact@{ history } =
                  contact {
                        history = DA.snoc history $  {
                              status: Received,
                              sender: userID,
                              recipient: recipientID,
                              id,
                              content,
                              date
                        }
                  }
            impersonationID = case experimenting of
                  Just (ImpersonationPayload { id }) -> Just id
                  _ -> Nothing

            findAndUpdateContactList = do
                  index <- DA.findIndex (findContact userID impersonationID model.experimenting) contacts
                  { impersonating } <- contacts !! index
                  let updated = DA.modifyAt index (updateHistory { content, id, date }) contacts
                  --if impersonating, only the user can start new chats
                  case model.experimenting, experimenting, impersonating of
                        Nothing, Just (ImpersonationPayload _), Nothing -> Nothing
                        _, _, _ -> updated

findContact :: Int -> Maybe Int -> Maybe ExperimentData -> Contact -> Boolean
findContact userID impersonationID experimenting { user: { id }, impersonating } = userID == id && (DM.isJust experimenting || impersonating == impersonationID)

updateTemporaryID :: Array Contact -> Int -> Int -> Int -> Array Contact
updateTemporaryID contacts userID previousMessageID messageID = updateContactHistory contacts userID updateTemporary
      where updateTemporary history@( { id })
                  | id == previousMessageID = history { id = messageID, status = Received }
                  | otherwise = history

updateStatus :: Array Contact -> Int -> Array Int -> MessageStatus -> Array Contact
updateStatus contacts userID ids status = updateContactHistory contacts userID updateSt
      where updateSt history@( { id })
                  | DA.elem id ids = history { status = status }
                  | otherwise = history

markErroredMessage :: Array Contact -> Int -> Int -> Array Contact
markErroredMessage contacts userID messageID = updateContactHistory contacts userID updateStatus
      where updateStatus history@( { id })
                  | id == messageID = history { status = Errored }
                  | otherwise = history

--refactor: should be abstract with updateReadCount
updateContactHistory :: Array Contact -> Int -> (HistoryMessage -> HistoryMessage) -> Array Contact
updateContactHistory contacts userID f = updateContact <$> contacts
      where updateContact contact@{ user: { id }, history }
                  | id == userID = contact {
                        history = f <$> history
                  }
                  | otherwise = contact

markContactUnavailable :: Array Contact -> Int -> Array Contact
markContactUnavailable contacts userID = updateContact <$> contacts
      where updateContact contact@{ user: { id }, history }
                  | id == userID = contact {
                        available = false
                  }
                  | otherwise = contact

checkMissedEvents :: IMModel -> MoreMessages
checkMissedEvents model@{ experimenting, contacts, user : { id } } =
      model :> if DM.isJust experimenting then [] else [do
            let { lastSentMessageID, lastReceivedMessageID} = findLastMessages contacts id

            if DM.isNothing lastSentMessageID && DM.isNothing lastReceivedMessageID then
                  pure Nothing
             else
                  CCNT.retryableResponse CheckMissedEvents ResumeMissedEvents (request.im.missedEvents { query: { lastSenderID: lastSentMessageID, lastRecipientID: lastReceivedMessageID } })
      ]

findLastMessages :: Array Contact -> Int -> { lastSentMessageID :: Maybe Int, lastReceivedMessageID :: Maybe Int }
findLastMessages contacts sessionUserID = {
      lastSentMessageID: findLast (\h -> sessionUserID == h.sender && h.status == Received),
      lastReceivedMessageID: findLast ((sessionUserID /= _) <<< _.sender)
}
      where findLast f = do
                  index <- DA.findLastIndex f allHistories
                  { id } <- allHistories !! index
                  pure id

            allHistories = DA.sortBy byID <<< DA.concatMap _.history $ DA.filter (DM.isNothing <<< _.impersonating) contacts
            byID { id } { id: anotherID } = compare id anotherID

setName :: String -> IMModel -> NoMessages
setName name model@{ user } =
      F.noMessages $ model {
            user = user {
                  name = name
            }
      }

toggleConnectedWebSocket :: Boolean -> IMModel -> MoreMessages
toggleConnectedWebSocket isConnected model@{ hasTriedToConnectYet, isWebSocketConnected, errorMessage } =
      model {
            hasTriedToConnectYet = true,
            isWebSocketConnected = isConnected,
            errorMessage = if not isConnected then lostConnectionMessage else if errorMessage == lostConnectionMessage then "" else errorMessage
      } :> if hasTriedToConnectYet && isConnected then [pure <<< Just $ SpecialRequest CheckMissedEvents] else []
      where lostConnectionMessage = "Connection to the server lost. Attempting to automaticaly reconnect..."

preventStop :: Event -> IMModel -> NextMessage
preventStop event model = CIF.nothingNext model <<< liftEffect $ CCD.preventStop event

checkNotifications :: Effect Unit
checkNotifications = do
      status <- CCD.notificationPermission
      when (status == "default") $ FS.send imID ToggleAskNotification

--refactor use popstate subscription
historyChange :: Boolean -> Effect Unit
historyChange smallScreen = do
      popStateListener <- WET.eventListener $ const handler
      window <- WH.window
      WET.addEventListener popstate popStateListener false $ WHW.toEventTarget window
      where handler = do
                  CCD.pushState $ routes.im.get {}
                  when smallScreen <<< FS.send imID $ ToggleInitialScreen true

setUpWebSocket :: Ref CurrentWebSocket -> Effect Unit
setUpWebSocket webSocketRef = do
      { webSocket } <- ER.read webSocketRef
      let webSocketTarget = CIW.toEventTarget webSocket
      --a ref is used to track reconnections and ping intervals
      timerIDs <- ER.new { reconnectID : Nothing, pingID : Nothing }
      openListener <- WET.eventListener (open timerIDs)
      messageListener <- WET.eventListener runMessage
      closeListener <- WET.eventListener (close timerIDs)
      WET.addEventListener onMessage messageListener false webSocketTarget
      WET.addEventListener onOpen openListener false webSocketTarget
      WET.addEventListener onClose closeListener false webSocketTarget

      where open timerIDs _ = do
                  { reconnectID } <- ER.read timerIDs
                  case reconnectID of
                        Nothing -> pure unit
                        Just id -> do
                              ET.clearTimeout id
                              ER.modify_ ( _ { reconnectID = Nothing }) timerIDs
                  pong true
                  newPingID <- ping
                  ER.modify_ ( _ { pingID = Just newPingID }) timerIDs
                  FS.send imID $ ToggleConnected true
                  askForUpdates

            ping = ET.setInterval (1000 * 60) do
                  { webSocket, ponged } <- ER.read webSocketRef
                  if ponged then do
                        pong false
                        CIW.sendPayload webSocket Ping
                   else
                        CIW.close webSocket

            runMessage event = do
                  let payload = SU.fromRight <<< CME.runExcept <<< FO.readString <<< CIW.data_ <<< SU.fromJust $ CIW.fromEvent event
                      message = SU.fromRight $ SJ.fromJSON payload
                  isFocused <- CCD.documentHasFocus
                  case message of
                        Pong -> pong true
                        Content cnt -> FS.send imID $ ReceiveMessage cnt isFocused

            pong whether = ER.modify_ (_ { ponged = whether }) webSocketRef

            askForUpdates = do
                  { webSocket} <- ER.read webSocketRef
                  CIW.sendPayload webSocket UpdateHash

            close timerIDs _ = do
                  FS.send imID $ ToggleConnected false
                  { reconnectID, pingID } <- ER.read timerIDs
                  newPingID <- case pingID of
                        Nothing -> pure unit
                        Just id -> do
                              ET.clearInterval id
                              ER.modify_ (_ { pingID = Nothing }) timerIDs
                  when (DM.isNothing reconnectID) do
                        milliseconds <- ERD.randomInt 2000 10000
                        id <- ET.setTimeout milliseconds <<< void $ do
                              newWebSocket <- CIW.createWebSocket
                              ER.modify_ (_ { webSocket = newWebSocket }) webSocketRef
                              setUpWebSocket webSocketRef
                        ER.modify_ (_ { reconnectID = Just id }) timerIDs

setSmallScreen :: IMModel -> NoMessages
setSmallScreen model@{ messageEnter } =
      F.noMessages $ model {
            messageEnter = false,
            smallScreen = true
      }
