module Shared.IM.View.Suggestions where

import Prelude
import Shared.Types

import Data.Array ((!!))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.IM.Svg as SIS
import Shared.IM.Types
import Shared.Avatar as SA

suggestions :: IMModel -> Html IMMessage
suggestions { contacts, suggesting, chatting, suggestions }
      | DM.isJust chatting =
            HE.div (HA.class' "side-suggestions-container") [
                  HE.div [HA.class'"side-suggestion back-side-suggestion", HA.onClick ResumeSuggesting, HA.title "Browse chat suggestions again"] [
                        SIS.arrow [HA.class' "svg-back-suggestions"],
                        HE.span (HA.class' "back-suggestions") "Back to chat suggestions"
                  ]
            ]
      | otherwise =
            case suggs of
                  Just { avatar, name } | not $ DA.null contacts -> HE.div (HA.class' "side-suggestions-container") [
                        HE.div [HA.class'"side-suggestion", HA.title "Your chat suggestions"] [
                              HE.div (HA.class' "avatar-contact-list-div faded") [
                                    let previousIndex = map (_ - 1) suggesting in
                                          HE.img [HA.class' $ "avatar-contact-list" <> SA.avatarColorClass previousIndex, HA.src $ SA.avatarForRecipient previousIndex $ getAvatar previousIndex]
                              ],
                              HE.div (HA.class' "avatar-contact-list-div margin-less-z") [
                                    HE.img [HA.class' $ "avatar-contact-list" <> SA.avatarColorClass suggesting, HA.src $ SA.avatarForRecipient suggesting avatar]
                              ],
                              HE.div (HA.class' "avatar-contact-list-div margin-less faded") [
                                    let nextIndex = map (_ + 1) suggesting in
                                          HE.img [HA.class' $ "avatar-contact-list" <> SA.avatarColorClass nextIndex, HA.src $ SA.avatarForRecipient nextIndex $ getAvatar nextIndex]
                              ],

                              HE.div [HA.class' "contact-profile"] [
                                    HE.span (HA.class' "contact-name") name
                              ]
                        ]
                  ]
                  _ -> HE.div' (HA.class'"side-suggestions-container" )

      where suggs = do
                  index <- suggesting
                  suggestions !! index

            getAvatar index = do
                  i <- index
                  user <- suggestions !! i
                  user.avatar
