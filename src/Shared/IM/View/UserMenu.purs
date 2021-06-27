module Shared.IM.View.UserMenu where

import Prelude
import Shared.Types

import Shared.Experiments.Types
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Shared.IM.Types
import Flame (Html)
import Shared.IM.Types
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.IM.Svg as SIS

userMenu :: IMModel -> Html IMMessage
userMenu model@{ toggleContextMenu, smallScreen, experimenting } =
      HE.div (HA.class' "settings") [
            header model,
            HE.svg [HA.class' {"svg-experiment" : true, "svg-imp": DM.isJust experimenting }, HA.viewBox "0 0 1479 1536", HA.onClick <<< SpecialRequest $ ToggleModal ShowExperiments] [
                  HE.title "Chat experiments",
                  --from https://leungwensen.github.io/svg-icon/#awesome
                  HE.path' $ HA.d "M1434.5 1320q56 89 21.5 152.5t-140.5 63.5h-1152q-106 0-140.5-63.5T44.5 1320l503-793V128h-64q-26 0-45-19t-19-45 19-45 45-19h512q26 0 45 19t19 45-19 45-45 19h-64v399zm-779-725l-272 429h712l-272-429-20-31V128h-128v436z"
            ],

            HE.div [HA.class' "outer-user-menu", HA.title "Your options"] [
                  HE.svg [HA.id $ show UserContextMenu, HA.class' "svg-32 svg-user-menu-context", HA.viewBox "0 0 16 16"] (SIS.contextMenuElements <> [
                        HE.rect' [HA.class' "strokeless", HA.x "0.03", HA.y "7", HA.width "15.93", HA.height "2"],
                        HE.rect' [HA.class' "strokeless", HA.x "0.03", HA.y "2.5", HA.width "15.93", HA.height "2"],
                        HE.rect' [HA.class' "strokeless", HA.x "0.03", HA.y "11.5", HA.width "15.93", HA.height "2"]
                  ]),
                  HE.div [HA.class' {"user-menu": true, visible: toggleContextMenu == ShowUserContextMenu }][
                        HE.div (HA.class' "mobile-profile-header") $ header model,
                        HE.div [HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile] [
                              HE.div (HA.class' "menu-item-heading") "Profile",
                              HE.span (HA.class' "duller") "Set your profile picture, name"
                        ],
                        HE.div [HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowSettings] [
                              HE.div (HA.class' "menu-item-heading") "Settings",
                              HE.span (HA.class' "duller") "Change email, password, etc"
                        ],
                        HE.div [HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowBacker] [
                              HE.div (HA.class' "menu-item-heading") "Backing",
                              HE.span (HA.class' "duller") "Donate or become a patron"
                        ],
                        HE.div [HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowLeaderboard] [
                              HE.div (HA.class' "menu-item-heading") "Karma leaderboard",
                              HE.span (HA.class' "duller") "See your karma rank and stats"
                        ],
                        HE.div [HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowExperiments] [
                              HE.div (HA.class' "menu-item-heading") "Chat experiments",
                              HE.span (HA.class' "duller") "Talk in novel ways"
                        ],
                        HE.div [HA.class' "user-menu-item", HA.onClick <<< SpecialRequest $ ToggleModal ShowHelp] [
                              HE.div (HA.class' "menu-item-heading") "Help",
                              HE.span (HA.class' "duller") "Learn more about MelanChat"
                        ],
                        HE.div [HA.class' "user-menu-item logout menu-item-heading", HA.onClick <<< SpecialRequest $ ToggleModal ConfirmLogout] "Logout"
                  ]
            ],
            HE.span [HA.class' "suggestions-button", HA.onClick $ ToggleInitialScreen false] $
                  HE.svg [HA.class' "svg-suggestion-button", HA.viewBox "0 0 16 16"] [
                        HE.path' [HA.fill "#c3d365", HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM4.42,10l-.06.07-.49.59,0,.05-.93,1.13V9.54h0V3.41a.68.68,0,0,1,.61-.73h8.14a.67.67,0,0,1,.59.73V8.86a.72.72,0,0,1-.64.78H4.72Zm9.18,3.33-.92-1-.52-.57-.41-.45H4.9l.76-1h6.15a1,1,0,0,0,1-.9V5.11H13a.63.63,0,0,1,.61.66v5.49h0Z"],
                        HE.path' [HA.fill "#262626", HA.class' "strokeless",HA.d "M12.26,8.86V3.41a.67.67,0,0,0-.59-.73H3.53a.68.68,0,0,0-.61.73V9.54h0v2.31l.93-1.13,0-.05.49-.59L4.42,10l.3-.37h6.9A.72.72,0,0,0,12.26,8.86Zm-1-4.11A3.62,3.62,0,0,1,7.68,8.23,3.66,3.66,0,0,1,5.11,7.16a3.59,3.59,0,0,1-1.05-2.4V4.1h7.26Z"],
                        HE.path' [HA.fill "#c3d365", HA.class' "strokeless",HA.d "M4.06,4.76a3.59,3.59,0,0,0,1.05,2.4A3.66,3.66,0,0,0,7.68,8.23,3.62,3.62,0,0,0,11.3,4.75l0-.65H4.06Zm5.55.48a.43.43,0,1,1-.43.43A.43.43,0,0,1,9.61,5.24Zm-1.93,0a.43.43,0,0,1,0,.86.43.43,0,0,1,0-.86Zm-1.92,0a.43.43,0,0,1,0,.86.43.43,0,0,1,0-.86Z"],
                        HE.path' [HA.fill "#262626", HA.class' "strokeless",HA.d "M5.76,6.1a.43.43,0,0,0,0-.86.43.43,0,0,0,0,.86Z"],
                        HE.path' [HA.fill "#262626", HA.class' "strokeless",HA.d "M7.68,6.1a.43.43,0,0,0,0-.86.43.43,0,0,0,0,.86Z"],
                        HE.path' [HA.fill "#262626", HA.class' "strokeless",HA.d "M9.61,6.1a.43.43,0,1,0-.43-.43A.43.43,0,0,0,9.61,6.1Z"],
                        HE.path' [HA.fill "#262626", HA.class' "strokeless",HA.d "M13.59,5.77A.63.63,0,0,0,13,5.11h-.2V9.38a1,1,0,0,1-1,.9H5.66l-.76,1h6.85l.41.45.52.57.92,1V11.26h0Z"]
                  ]
      ]

header :: IMModel -> Html IMMessage
header model@{ user: { karma, karmaPosition }, experimenting } = HE.fragment [
      HE.img [HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile, HA.title "Edit your profile", HA.class' "avatar-settings", HA.src $ SA.avatarForSender avatar],
      HE.div [HA.class' "settings-name"] [
            HE.strong_ name,
            HE.div [HA.class' "settings-karma", HA.onClick <<< SpecialRequest $ ToggleModal ShowLeaderboard, HA.title "See karma leaderboard"] [
                  HE.span [HA.class' "karma-number"] $ show karma,
                  HE.span [HA.class' "karma-text"] " karma ",
                  HE.span_ $ "(#" <> show karmaPosition <> ")"
            ]
      ]
]
      where { name, avatar } = case experimenting of
                  Just (Impersonation (Just profile)) -> profile
                  _ -> model.user