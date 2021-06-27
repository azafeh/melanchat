module Server.IM.Template where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.HashSet as DHS
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Environment (emojiJSHash, imCSSHash, imJSHash)
import Flame (QuerySelector(..))
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Server.Template (defaultParameters)
import Server.Template as ST
import Shared.IM.Unread as SIU
import Shared.IM.Types
import Shared.IM.View as SIV
import Shared.Path (updateHash)
import Shared.Path as SP

template :: {
      contacts :: Array Contact,
      suggestions :: Array Suggestion,
      user :: IMUser
} -> Effect String
template {contacts, suggestions, user} = do
      let   unreadChats = SIU.countUnreadChats user.id contacts
            suggestionsCount = DA.length suggestions
      F.preMount (QuerySelector ".im") {
            view: \model -> ST.templateWith $ defaultParameters {
                  title = SIU.title unreadChats,
                  favicon = SIU.favicon unreadChats,
                  content = [SIV.view false model],
                  javascript = javascript,
                  css = css
            },
            init: {
                  chatting: Nothing,
                  freeToFetchSuggestions: true,
                  temporaryID: 0,
                  modalsLoaded: [],
                  suggesting: if suggestionsCount == 0 then Nothing else if suggestionsCount == 1 then Just 0 else Just 1,
                  freeToFetchChatHistory:true,
                  suggestionsPage: 0,
                  errorMessage: "",
                  smallScreen: false,
                  initialScreen: true,
                  enableNotificationsVisible: false,
                  messageEnter: true,
                  imageCaption: Nothing,
                  link: Nothing,
                  linkText: Nothing,
                  selectedImage: Nothing,
                  fullContactProfileVisible: false,
                  freeToFetchContactList: true,
                  erroredFields: [],
                  fortune: Nothing,
                  toggleContextMenu: HideContextMenu,
                  toggleModal: HideUserMenuModal,
                  toggleChatModal: HideChatModal,
                  experimenting: Nothing,
                  blockedUsers: [],
                  reportReason: Nothing,
                  reportComment: Nothing,
                  imUpdated: false,
                  isWebSocketConnected: false,
                  failedRequests: [],
                  hasTriedToConnectYet: false,
                  hash: updateHash,
                  contacts,
                  suggestions,
                  user
            }
      }
      where javascript = [
                  HE.script' [HA.type' "text/javascript", HA.src <<< SP.pathery JS $ "emoji." <> emojiJSHash],
                  HE.script' [HA.type' "text/javascript", HA.src <<< SP.pathery JS $  "im." <> imJSHash]
            ]
            css = [
                  HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href <<< SP.pathery CSS $ "im." <> imCSSHash],
                  HE.style (HA.type' "text/css") """.suggestion.new {
                        background: url(https://static.melan.chat/file/ourmelon/suggestions.png);
                  }"""
            ]
