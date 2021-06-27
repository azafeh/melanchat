module Shared.IM.View.Contacts where

import Prelude
import Shared.Types

import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Enum as DE
import Data.Foldable as DF
import Data.HashMap as DH
import Shared.Experiments.Types
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.DateTime as SD
import Shared.Experiments.Impersonation (impersonations)
import Shared.Experiments.Impersonation as SEI
import Shared.IM.View.Profile as SIVP
import Shared.IM.View.Retry as SIVR
import Shared.IM.Types
import Shared.Markdown as SM
import Shared.Unsafe as SU

contactList :: Boolean -> IMModel -> Html IMMessage
contactList isClientRender { failedRequests, chatting, experimenting, contacts, user: { id: userID } } = HE.div [HA.onScroll CheckFetchContacts, HA.class' "contact-list", HA.id "contact-list"] $ retryMissedEvents : DA.snoc allContacts retryFetchContacts
      where --the ordering of the contact list is only done for the dom nodes
            -- model.contacts is left unchanged
            allContacts
                  | DA.null contacts = [suggestionsCall]
                  | otherwise = DA.mapWithIndex contactEntry $ DA.sortBy compareDateUnread contacts

            suggestionsCall =
                  let { welcome, first, second } = case experimenting of
                        Just (Impersonation (Just { name })) -> SEI.welcomeMessage name
                        _ -> { welcome: "Welcome!", first: "Tap on either of the arrows to see ", second: "your chat suggestions" }
                  in HE.div (HA.class' "suggestions-call") [
                        HE.div (HA.onClick $ ToggleInitialScreen false) $ SIVP.backArrow,
                        HE.div (HA.class' {"suggestions-call-middle": true, "welcome-imp": DM.isJust experimenting}) [
                              HE.div (HA.class' "welcome-suggestions-call") $ welcome,
                              HE.div_ first,
                              HE.div_ second
                        ],
                        HE.div (HA.onClick $ ToggleInitialScreen false) $ SIVP.nextArrow
                  ]

            getDate history = do
                  { date: DateTimeWrapper md } <- DA.last history
                  pure md
            compareDateUnread contact anotherContact = compare (getDate anotherContact.history) (getDate contact.history)

            unread total { status, sender } = total + DE.fromEnum (sender /= userID && status < Read)
            countUnread = DF.foldl unread 0

            lastMessage = DM.fromMaybe "" <<< map _.content
            lastStatus = DM.fromMaybe "" <<< map (show <<< _.status)
            lastDate = DM.fromMaybe "" <<< map (SD.ago <<< DN.unwrap <<< _.date)
            chattingID = do
                  index <- chatting
                  { user: { id } } <- contacts !! index
                  pure id
            impersonatingID =  do
                  index <- chatting
                  { impersonating } <- contacts !! index
                  impersonating

            --it is using the history for the existing contact, not the impersonation
            contactEntry index { history, user, impersonating } =
                  let   index' = Just index
                       --refactor: a neater way to do experiment that dont litter the code with case of
                        userProfile = case impersonating of
                              Just impersonationID -> SU.fromJust $ DH.lookup impersonationID impersonations
                              _ -> user
                        numberUnreadMessages = countUnread history
                        maybeMessage = DA.last history

                  in HE.div [HA.class' { contact: true, "chatting-contact": chattingID == Just user.id && impersonatingID == impersonating }, HA.onClick <<< ResumeChat $ Tuple user.id impersonating] [
                        HE.div (HA.class' "avatar-contact-list-div") [
                              HE.img [HA.class' $ "avatar-contact-list" <> SA.avatarColorClass index', HA.src $ SA.avatarForRecipient index' userProfile.avatar]
                        ],
                        HE.div [HA.class' "contact-profile"] [
                              HE.span (HA.class' "contact-name") userProfile.name,
                              HE.div' [HA.class' "contact-list-last-message", HA.innerHtml <<< SM.parseRestricted $ lastMessage maybeMessage]
                        ],
                        HE.div (HA.class' "contact-options") [
                              HE.span (HA.class' {duller: true, invisible: not isClientRender }) $ lastDate maybeMessage,
                              HE.div (HA.class' { "unread-messages" :true, hidden: numberUnreadMessages == 0}) <<< HE.span (HA.class' "unread-number") $ show numberUnreadMessages,
                              HE.div (HA.class' { duller: true, hidden: numberUnreadMessages > 0 || map _.sender maybeMessage == Just user.id }) $ lastStatus maybeMessage
                        ]
                  ]

            retryMissedEvents = SIVR.retry "Failed to sync contacts. You might have missed messages." CheckMissedEvents failedRequests
            retryFetchContacts = SIVR.retry "Failed to load contacts" (FetchContacts true) failedRequests
