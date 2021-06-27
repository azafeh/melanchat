module Shared.IM.View.Notification where

import Prelude
import Shared.Types

import Flame (Html)
import Shared.IM.Types
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.IM.View.Retry as SIR

reloadPage :: Boolean -> Html IMMessage
reloadPage shouldReload = HE.div [HA.class' {"notification": true, hidden: not shouldReload}, HA.onClick ReloadPage] [
      SIR.retryIcon "notification-icon",
      HE.div (HA.class' "notification-message") [
            HE.strong_ "Update available",
            HE.br,
            HE.span_ "Click to update MelanChat"
      ]
]

prompt :: Boolean -> Html IMMessage
prompt enableNotificationsVisible = HE.div [HA.class' { notification: true, hidden: not enableNotificationsVisible }, HA.onClick AskNotification] [
      HE.svg [HA.class' "notification-icon", HA.viewBox "0 0 16 16"] [
            HE.path' [ HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM8,15.51A7.51,7.51,0,1,1,15.5,8,7.51,7.51,0,0,1,8,15.51Z"]
      ],
      HE.div (HA.class' "notification-message") [
            HE.strong_ "Get notified of new messages",
            HE.br,
            HE.span_ "Click to enable notifications"
      ]
]
