module Test.Client.Model where

import Prelude
import Shared.Types

import Data.HashMap (HashMap)
import Data.HashMap as HS
import Data.HashSet as DHS
import Shared.IM.Types
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.IM.Contact as SIC
import Unsafe.Coerce as UC
import Web.DOM (Element)
import Web.Socket.WebSocket (WebSocket)

run :: forall m. m -> Aff (m -> m) -> Aff m
run model f = do
      f' <- f
      pure $ f' model

model :: IMModel
model = {
      modalsLoaded : [],
      freeToFetchSuggestions: true,
      initialScreen: true,
      suggestionsPage: 0,
      hash: "",
      freeToFetchContactList: true,
      errorMessage: "",
      reportReason: Nothing,
      reportComment: Nothing,
      imUpdated : false,
      smallScreen: false,
      failedRequests: [],
      toggleContextMenu: HideContextMenu,
      imageCaption: Nothing,
      fullContactProfileVisible: false,
      experimenting : Nothing,
      fortune: Nothing,
      link: Nothing,
      erroredFields: [],
      hasTriedToConnectYet: false,
      selectedImage: Nothing,
      messageEnter: true,
      linkText: Nothing,
      blockedUsers: [],
      toggleModal: HideUserMenuModal,
      toggleChatModal: HideChatModal,
      enableNotificationsVisible: false,
      user: imUser,
      suggestions: [suggestion],
      temporaryID: 0,
      suggesting: Just 0,
      freeToFetchChatHistory: true,
      contacts: [contact],
      isWebSocketConnected: true,
      chatting: Just 0
}

imUserID :: Int
imUserID = 23

imUser :: IMUser
imUser = {
      karmaPosition: 1,
      age: Nothing,
      name: "test",
      id: imUserID,
      avatar: Nothing,
      country: Nothing,
      languages: [],
      tags: [],
      headline: "",
      description: "",
      gender: Nothing,
      karma: 5
}

anotherIMUserID :: Int
anotherIMUserID = 90

contactID :: Int
contactID = anotherIMUserID

anotherIMUser :: IMUser
anotherIMUser = imUser { id = anotherIMUserID }

contact :: Contact
contact = SIC.defaultContact imUserID anotherIMUser

suggestionID :: Int
suggestionID =  300

suggestion :: Suggestion
suggestion = imUser { id = suggestionID }

historyMessage :: HistoryMessage
historyMessage = {
      id: 1,
      sender: imUserID,
      recipient: contactID,
      date: DateTimeWrapper <<< EU.unsafePerformEffect $ EN.nowDateTime,
      content: "test",
      status: Received
}

webSocket :: WebSocket
webSocket = UC.unsafeCoerce 2

elements :: HashMap ElementID Element
elements = HS.empty