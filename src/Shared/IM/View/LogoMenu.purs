module Shared.IM.View.LogoMenu where

import Prelude
import Shared.Types

import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.String as DS
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.IM.Types
import Shared.Path as SP

logoMenu :: Maybe String -> Html IMMessage
logoMenu fortune = HE.div (HA.class' "relative") [
      HE.div (HA.class' "tabs") [
            HE.div (HA.class' "tab-item duller") [
                  HE.svg [HA.class' "svg-32", HA.viewBox "0 0 16 16"] [
                        HE.path' [HA.class' "strokeless", HA.d "M2.29,11.1l.45-.55H13.19a1.1,1.1,0,0,0,1-1.18V1.1A1,1,0,0,0,13.27,0H.94A1,1,0,0,0,0,1.11v9.28H0v3.5l1.4-1.7.06-.08.73-.89Zm-.95-.43-.11.13L1,11.1v-10s0,0,0,0H13.22s0,0,0,0V9.37a.42.42,0,0,1,0,.11H2.32l0,0-.1.12L2,9.83Z"],
                        HE.path' [HA.class' "strokeless", HA.d "M16,12.74V4.11a1,1,0,0,0-1-1h-.52v1H15s0,0,0,0v9.3l-.26-.28-.82-.91L13.74,12l-.13-.14H2.86l-.86,1H13.17l.57.63.81.9L16,16V12.74Z"],
                        HE.rect' [HA.class' "strokeless", HA.x "3.81", HA.y "2.94", HA.width "7.23", HA.height "0.75"],
                        HE.rect' [HA.class' "strokeless", HA.x "3.81", HA.y "4.69", HA.width "7.23", HA.height "0.75"],
                        HE.rect' [HA.class' "strokeless", HA.x "3.81", HA.y "6.44", HA.width "7.23", HA.height "0.75"]
                  ],
                  HE.text "Contacts"
            ],
            HE.div [HA.class' "tab-item duller", HA.onClick $ ToggleInitialScreen false] [
                   HE.svg [HA.class' "svg-32 svg-duller", HA.viewBox "0 0 16 16"] [
                        HE.path' [HA.class' "strokeless", HA.d "M2.29,11.1l.45-.55H13.19a1.1,1.1,0,0,0,1-1.18V1.1A1,1,0,0,0,13.27,0H.94A1,1,0,0,0,0,1.11v9.28H0v3.5l1.39-1.7.06-.08.73-.89Zm-.95-.43-.11.13L1,11.1v-10s0,0,0,0H13.22s0,0,0,0V9.37a.42.42,0,0,1,0,.11H2.32l0,0-.1.12L2,9.83Z"],
                        HE.path' [HA.class' "strokeless", HA.d "M16,12.74V4.11a1,1,0,0,0-1-1h-.52v1H15s0,0,0,0v9.3l-.26-.28-.82-.91L13.74,12l-.13-.14H2.86l-.86,1H13.17l.57.63.81.9L16,16V12.74Z"],
                        HE.path' [HA.class' "strokeless", HA.d "M7.23,8.41a5.5,5.5,0,0,0,5.49-5.27l0-1h-11v1a5.46,5.46,0,0,0,1.6,3.64A5.54,5.54,0,0,0,7.23,8.41Zm4.49-5.25A4.5,4.5,0,0,1,7.23,7.41,4.57,4.57,0,0,1,4,6.08a4.44,4.44,0,0,1-1.3-2.92Z"],
                        HE.path' [HA.class' "strokeless", HA.d "M4.86,5.06a.52.52,0,0,0,.53-.53A.53.53,0,0,0,4.86,4a.53.53,0,0,0-.53.53A.53.53,0,0,0,4.86,5.06Z"],
                        HE.path' [HA.class' "strokeless", HA.d "M7.23,5.06a.52.52,0,0,0,.53-.53.53.53,0,1,0-1,0A.52.52,0,0,0,7.23,5.06Z"],
                        HE.path' [HA.class' "strokeless", HA.d "M9.61,5.06a.53.53,0,0,0,.53-.53A.53.53,0,0,0,9.61,4a.53.53,0,0,0-.53.53A.52.52,0,0,0,9.61,5.06Z"]
                  ],
                  HE.text "Suggestions"
            ]
      ],
      HE.div (HA.class' {fortune: true, hidden: DM.isNothing fortune }) [
            HE.div (HA.class' "fortune-deets") [
                  HE.text $ DM.fromMaybe "" fortune
            ],
            HE.svg [HA.viewBox "0 0 512 512", HA.onClick (ToggleFortune false) ] [
                  HE.title "Close",
                  HE.polygon' $ HA.points "438.627 118.627 393.373 73.373 256 210.746 118.627 73.373 73.373 118.627 210.746 256 73.373 393.373 118.627 438.627 256 301.254 393.373 438.627 438.627 393.373 301.254 256 438.627 118.627"
            ]
      ],

      HE.div [HA.class' "logo-contact-list", HA.onDblclick $ ToggleFortune true] $
            HE.img [
                  HA.createAttribute "srcset" $ DS.joinWith " " [SP.pathery PNG "logo-3-small", "180w,", SP.pathery PNG "logo-small", "210w"],
                  HA.createAttribute "sizes" "(max-width: 1920px) 180px, 210px",
                  HA.src $ SP.pathery PNG "logo"
            ]
]