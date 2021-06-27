module Shared.IM.View.History where

import Prelude
import Shared.Types

import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Types
import Shared.Avatar as SA
import Shared.IM.Types
import Shared.DateTime as SD
import Shared.IM.View.Retry as SIVR
import Shared.Markdown as SM

history :: IMModel -> Maybe Contact -> Html IMMessage
history { user: { id: senderID, avatar: senderAvatar }, experimenting, chatting, failedRequests, freeToFetchChatHistory } contact =
      HE.div [HA.class' { "message-history": true, hidden: DM.isNothing contact }, HA.id "message-history", HA.onScroll CheckFetchHistory] chatHistory
      where chatHistory =
                  case contact of
                        Nothing -> [retryOrWarning]
                        Just recipient@{ shouldFetchChatHistory, available } ->
                              if available then
                                    let entries = retryOrWarning : display recipient
                                    in if shouldFetchChatHistory || not freeToFetchChatHistory then HE.div' (HA.class' "loading") : entries else entries
                              else
                                    []

            retryOrWarning = case experimenting of
                  Just (Impersonation (Just { name })) ->
                        HE.div (HA.class' "imp impersonation-warning-history")  [
                              HE.div_ [
                                    HE.text "You are impersonating ",
                                    HE.strong_ name
                              ]
                        ]
                  _ ->
                        SIVR.retry "Failed to load chat history" (FetchHistory true) failedRequests
            display recipient@{ history, user: { avatar } } = DA.mapWithIndex (\i -> entry avatar (map _.sender (history !! (i - 1)))) history

            entry recipientAvatar previousSender { id, status, date, sender, content } =
                  let sameSender = sender == senderID
                      avatar =
                        if senderID == sender then
                              SA.avatarForSender senderAvatar
                         else
                              SA.avatarForRecipient chatting recipientAvatar
                  in HE.div (HA.class' {
                        message: true,
                        "sender-message": sameSender,
                        "recipient-message": not sameSender,
                        "no-avatar-message": previousSender == Just sender
                  }) [
                        HE.div [HA.class' "message-content", HA.id $ "m" <> show id ] [
                              HE.div' [HA.innerHtml $ SM.parse content],
                              HE.div (HA.class' {
                                    duller: status /= Errored,
                                    "error-message": status == Errored,
                                    "message-status": true}) [
                                          HE.text <<< SD.agoWithTime $ DN.unwrap date,
                                          HE.span (HA.class' { hidden: not sameSender }) $ " - " <> show status
                              ]
                        ]
                  ]
