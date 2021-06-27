module Shared.IM.Types where

--refactor: split to the correct modules

import Control.Monad.Except as CME
import Data.Argonaut.Core as DAC
import Shared.Experiments.Types
import Data.Argonaut.Core as DAP
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Array as DA
import Data.Bifunctor as DB
import Data.DateTime (Date, DateTime)
import Data.DateTime as DTT
import Data.DateTime.Instant as DDI
import Droplet.Language as DL
import Droplet.Language (class FromValue)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Show.Generic as DGRS
import Data.Hashable (class Hashable)
import Data.Hashable as HS
import Data.Int as DI
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Number as DNM
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.Read (class Read)
import Data.String.Read as DSR
import Data.String.Regex as DSRG
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe as DSRU
import Data.Time.Duration as DTD
import Data.Traversable as DT
import Data.Tuple (Tuple)
import Shared.User
import Foreign (F, Foreign, ForeignError(..))
import Foreign as F
import Foreign.Object (Object)
import Foreign.Object as FO
import Payload.Client.QueryParams (class EncodeQueryParam)
import Payload.Server.QueryParams (class DecodeQueryParam, DecodeError(..))
import Shared.DateTime as SDT
import Shared.Options.File (imageBasePath)
import Shared.Unsafe as SU
import Simple.JSON (class ReadForeign, class WriteForeign)
import Web.Event.Internal.Types (Event)
import Prelude
import Shared.User


type IMUser = Record IU

newtype DateTimeWrapper = DateTimeWrapper DateTime


type Suggestion = IMUser

type BasicMessage fields = {
      id :: Int,
      experimenting :: Maybe ExperimentPayload |
      fields
}

type ClientMessagePayload = (BasicMessage (
      content :: String,
      userID :: Int,
      date :: DateTimeWrapper
))

type Contact = {
      shouldFetchChatHistory :: Boolean, -- except for the last few messages, chat history is loaded when clicking on a contact for the first time
      user :: IMUser,
      available :: Boolean,
      chatAge :: Number, --Days,
      chatStarter :: Int,
      impersonating :: Maybe Int,
      history :: Array HistoryMessage
}

type HistoryMessage = {
      id :: Int,
      sender :: Int,
      recipient :: Int,
      date :: DateTimeWrapper,
      content :: String,
      status :: MessageStatus
}

data MessageStatus =
      Errored |
      Sent |
      Received |
      Delivered |
      Read

type MessageIDTemporary = {
      id :: Int,
      temporaryID :: Int
}

type MissedEvents = {
      contacts :: Array Contact,
      messageIDs :: Array MessageIDTemporary
}


--refactor: these fields can be grouped into inner objects (eg. report: { reason, comment })
type IM = (
      suggestions :: Array Suggestion,
      contacts :: Array Contact,
      --in case a message from someone blocked was already midway
      blockedUsers :: Array Int,
      temporaryID :: Int,
      freeToFetchChatHistory :: Boolean,
      freeToFetchContactList :: Boolean,
      freeToFetchSuggestions :: Boolean,
      selectedImage :: Maybe String,
      imageCaption :: Maybe String,
      messageEnter :: Boolean,
      link :: Maybe String,
      suggestionsPage :: Int,
      linkText :: Maybe String,
      isWebSocketConnected :: Boolean,
      erroredFields :: Array String,
      fortune :: Maybe String,
      failedRequests :: Array RequestFailure,
      errorMessage :: String,
      experimenting :: Maybe ExperimentData,
      modalsLoaded :: Array ShowUserMenuModal,
      reportReason :: Maybe ReportReason,
      reportComment :: Maybe String,
      --the current logged in user
      user :: IMUser,
      --indexes
      suggesting :: Maybe Int,
      chatting :: Maybe Int,
      smallScreen :: Boolean,
      --used to signal that the page should be reloaded
      hash :: String,
      --visibility switches
      initialScreen :: Boolean, --used on mobile to switch screens
      hasTriedToConnectYet :: Boolean,
      fullContactProfileVisible :: Boolean,
      imUpdated :: Boolean,
      enableNotificationsVisible :: Boolean,
      toggleContextMenu :: ShowContextMenu,
      toggleModal :: ShowUserMenuModal,
      toggleChatModal :: ShowChatModal
)

type IMModel = Record IM

data ShowChatModal =
      HideChatModal |
      ShowSelectedImage |
      ShowPreview |
      ShowEmojis |
      ShowLinkForm

data ShowContextMenu =
      HideContextMenu |
      ShowUserContextMenu |
      ShowSuggestionContextMenu |
      ShowCompactProfileContextMenu |
      ShowFullProfileContextMenu

data ShowUserMenuModal =
      HideUserMenuModal |
      ConfirmLogout |
      ConfirmTermination |
      ShowExperiments |
      ShowProfile |
      ShowSettings |
      ShowLeaderboard |
      ShowHelp |
      ShowBacker |
      ShowReport Int


type Stats = {
    characters :: Number,
    interest :: Number
}

type Turn = {
    senderStats :: Stats,
    recipientStats:: Stats,
    chatAge :: Number, -- Days,
    replyDelay :: Number --Seconds
}

data ProfilePresentation =
      FullContactProfile |
      CurrentSuggestion |
      OtherSuggestion

data MessageContent =
      Image String String | --caption & base64
      Text String

data Markup =
      Bold |
      Italic |
      Strike |
      Heading |
      OrderedList |
      UnorderedList



type RequestFailure = {
      request :: RetryableRequest,
      errorMessage :: String
}

data RetryableRequest =
      FetchHistory Boolean |
      FetchContacts Boolean |
      CheckMissedEvents |
      ToggleModal ShowUserMenuModal |
      BlockUser Int |
      PreviousSuggestion |
      NextSuggestion |
      ReportUser Int


-- | Errors that should be reported back to the user
data ResponseError =
      BadRequest { reason :: String } |
      InternalError { reason :: String, context :: Maybe DatabaseError } |
      ExpiredSession

newtype DateWrapper = DateWrapper Date

instance dddFromValue :: FromValue DateWrapper where
      fromValue v = map DateWrapper (DL.fromValue v :: Either String Date)

data ReportReason = DatingContent | Harrassment | HateSpeech | Spam | OtherReason

data IMMessage =
      --history
      CheckFetchHistory |
      DisplayHistory (Array HistoryMessage)|
      --user menu
      ToggleInitialScreen Boolean |
      Logout |
      SetContextMenuToggle ShowContextMenu |
      SetModalContents (Maybe String) ElementID String |
      --contact
      ResumeChat (Tuple Int (Maybe Int)) |
      UpdateReadCount |
      CheckFetchContacts |
      DisplayContacts (Array Contact) |
      DisplayNewContacts (Array Contact) |
      DisplayImpersonatedContact Int HistoryMessage (Array Contact) |
      ResumeMissedEvents MissedEvents |
      --suggestion
      FetchMoreSuggestions |
      ResumeSuggesting |
      DisplayMoreSuggestions (Array Suggestion) |
      --chat
      SetSelectedImage (Maybe String) |
      ToggleContactProfile |
      DropFile Event |
      ToggleMessageEnter |
      FocusInput ElementID |
      EnterBeforeSendMessage Event |
      ForceBeforeSendMessage |
      ResizeChatInput Event |
      BeforeSendMessage MessageContent |
      SendMessage MessageContent DateTimeWrapper |
      SetMessageContent (Maybe Int) String |
      Apply Markup |
      SetSmallScreen |
      SetEmoji Event |
      InsertLink |
      --main
      AskChatExperiment |
      SetChatExperiment (Maybe ExperimentData) |
      ReloadPage |
      ToggleUserContextMenu Event |
      SpecialRequest RetryableRequest |
      ReceiveMessage WebSocketPayloadClient Boolean |
      PreventStop Event |
      AskNotification |
      ToggleAskNotification |
      SetNameFromProfile String |
      ToggleConnected Boolean |
      SetField (IMModel -> IMModel) |
      ToggleFortune Boolean |
      DisplayFortune String |
      RequestFailed RequestFailure |
      ToggleChatModal ShowChatModal

data WebSocketPayloadServer =
      UpdateHash |
      Ping |
      OutgoingMessage (BasicMessage (
            userID :: Int,
            content :: MessageContent,
            turn :: Maybe Turn
      )) |
      ChangeStatus {
            userID :: Int,
            status :: MessageStatus,
            persisting :: Boolean, -- in some cases status changs should be not persisted to the database
            --alternatively, update by user?
            ids :: Array Int
      } |
      ToBlock {
            id :: Int
      }

data ElementID =
      UserContextMenu |
      SuggestionContextMenu |
      CompactProfileContextMenu |
      FullProfileContextMenu |
      ImageFileInput |
      ChatInputSuggestion |
      ChatInput |
      ContactList |
      ImageFormCaption |
      PasswordDiv |
      ConfirmPasswordInput |
      LinkFormUrl |
      MessageHistory |
      Favicon |
      ProfileEditionRoot |
      ChatInputPreview |
      SettingsEditionRoot |
      KarmaLeaderboard |
      ExperimentsRoot |
      HelpRoot |
      TermsLink |
      PrivacyLink |
      Faq |
      TermsSection |
      PasswordInput |
      EmailDiv |
      PrivacySection |
      EmailInput |
      BackerRoot |
      ConfirmPassword |
      FaqLink |
      AvatarFileInput



data FullWebSocketPayloadClient =
      Pong |
      Content WebSocketPayloadClient

data WebSocketPayloadClient =
      CurrentHash String |
      NewIncomingMessage ClientMessagePayload |
      ServerReceivedMessage {
          previousID :: Int,
          id :: Int,
          userID :: Int
      } |
      ServerChangedStatus {
            ids :: Array Int,
            status :: MessageStatus,
            userID :: Int
      } |
      BeenBlocked { id :: Int } |
      PayloadError { origin :: WebSocketPayloadServer, context :: Maybe DatabaseError }

data DatabaseError = MissingForeignKey

instance showMessageStatus :: Show MessageStatus where
      show = case _ of
            Errored -> "Failed to send"
            Sent -> "Sending"
            Received -> "Sent"
            Delivered -> "Unread"
            Read -> "Read"
instance contentReportReason :: Show ReportReason where
      show = case _ of
            DatingContent -> "Dating content"
            Harrassment -> "Harrassment/Bullying"
            HateSpeech -> "Hate Speech/Call to violence"
            Spam -> "Spam/Product placement"
            OtherReason -> "Other"

derive instance ordReportReason :: Ord ReportReason
derive instance ordMessageStatus :: Ord MessageStatus


instance decodeJsonReportReason :: DecodeJson ReportReason  where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonMessageStatus :: DecodeJson MessageStatus where
      decodeJson = DADGR.genericDecodeJson

instance encodeJsonReportReason :: EncodeJson ReportReason where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonMessageStatus :: EncodeJson MessageStatus where
      encodeJson = DAEGR.genericEncodeJson

instance readForeignMessageStatus :: ReadForeign MessageStatus where
      readImpl value = SU.fromJust <<< DE.toEnum <$> F.readInt value
instance readForeignReportReason :: ReadForeign ReportReason where
      readImpl value = SU.fromJust <<< DE.toEnum <$> F.readInt value

instance writeForeignReportReason :: WriteForeign ReportReason where
      writeImpl reason = F.unsafeToForeign $ DE.fromEnum reason
instance writeForeignMessageStatus :: WriteForeign MessageStatus where
      writeImpl messageStatus = F.unsafeToForeign $ DE.fromEnum messageStatus

derive instance genericMessageStatus :: Generic MessageStatus _
derive instance genericReportReason :: Generic ReportReason _

derive instance eqReportReason :: Eq ReportReason
derive instance eqMessageStatus :: Eq MessageStatus

instance boundedMessageStatus :: Bounded MessageStatus where
      bottom = Received
      top = Read
instance enumReportReason :: Enum ReportReason where
      succ = case _ of
            DatingContent -> Just Harrassment
            Harrassment -> Just HateSpeech
            HateSpeech -> Just Spam
            Spam -> Just OtherReason
            OtherReason -> Nothing

      pred = case _ of
            DatingContent -> Nothing
            Harrassment -> Just DatingContent
            HateSpeech -> Just Harrassment
            Spam -> Just HateSpeech
            OtherReason -> Just Spam


instance boundedEnumMessageStatus :: BoundedEnum MessageStatus where
      cardinality = Cardinality 1

      fromEnum = case _ of
            Errored -> -1
            Sent -> 0
            Received -> 1
            Delivered -> 2
            Read -> 3

      toEnum = case _ of
            -1 -> Just Errored
            0 -> Just Sent
            1 -> Just Received
            2 -> Just Delivered
            3 -> Just Read
            _ -> Nothing

instance enumMessageStatus :: Enum MessageStatus where
      succ = case _ of
            Errored -> Just Received
            Sent -> Just Sent
            Received -> Just Delivered
            Delivered -> Just Read
            Read -> Nothing

      pred = case _ of
            Errored -> Nothing
            Sent -> Just Sent
            Received -> Just Errored
            Delivered -> Just Received
            Read -> Just Delivered

instance boundedReportReason :: Bounded ReportReason where
      bottom = DatingContent
      top = OtherReason

instance boundedEnumReportReason :: BoundedEnum ReportReason where
      cardinality = Cardinality 1

      fromEnum = case _ of
            DatingContent -> 0
            Harrassment -> 1
            HateSpeech -> 2
            Spam -> 3
            OtherReason -> 255

      toEnum = case _ of
            0 -> Just DatingContent
            1 -> Just Harrassment
            2 -> Just HateSpeech
            3 -> Just Spam
            255 -> Just OtherReason
            _ -> Nothing




instance decodeJsonMDateTime :: DecodeJson DateTimeWrapper where
      decodeJson = DM.maybe (Left $ DAD.TypeMismatch "couldnt parse epoch") (Right <<< DateTimeWrapper <<< DDI.toDateTime) <<< DAP.caseJsonNumber (Nothing) (DDI.instant <<< DTD.Milliseconds)
instance decodeJsonMDate :: DecodeJson DateWrapper where
      decodeJson = DM.maybe (Left $ DAD.TypeMismatch "couldnt parse epoch") (Right <<< DateWrapper <<< DTT.date <<< DDI.toDateTime) <<< DAP.caseJsonNumber (Nothing) (DDI.instant <<< DTD.Milliseconds)
instance decodeJsonWebSocketPayloadServer :: DecodeJson WebSocketPayloadServer where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonMessageContent :: DecodeJson MessageContent where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonShowModal :: DecodeJson ShowUserMenuModal where
      decodeJson = DADGR.genericDecodeJson


instance decodeJsonWebSocketPayloadClient :: DecodeJson WebSocketPayloadClient  where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonShowContextMenu :: DecodeJson ShowContextMenu where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonPayloadErrorContext :: DecodeJson DatabaseError where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonRetryableRequest :: DecodeJson RetryableRequest where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonShowChatModal :: DecodeJson ShowChatModal where
      decodeJson = DADGR.genericDecodeJson


instance encodeJsonMDateTime :: EncodeJson DateTimeWrapper where
      encodeJson = DAC.fromNumber <<< SDT.dateTimeToNumber
instance encodeJsonMDate :: EncodeJson DateWrapper where
      encodeJson = DAC.fromNumber <<< SDT.dateToNumber
instance encodeJsonWebSocketPayloadServer :: EncodeJson WebSocketPayloadServer where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonMessageContent :: EncodeJson MessageContent where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonShowModal :: EncodeJson ShowUserMenuModal where
      encodeJson = DAEGR.genericEncodeJson


instance encodeJsonWebSocketPayloadClient :: EncodeJson WebSocketPayloadClient where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonShowContextMenu :: EncodeJson ShowContextMenu where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonPayloadErrorContext :: EncodeJson DatabaseError where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonRetryableRequest :: EncodeJson RetryableRequest where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonShowChatModal :: EncodeJson ShowChatModal where
      encodeJson = DAEGR.genericEncodeJson



instance hashableIMSelector :: Hashable ElementID where
      hash = HS.hash <<< show

instance showResponseError :: Show ResponseError where
      show = DGRS.genericShow
instance showShowUserMenuModal :: Show ShowUserMenuModal where
      show = case _ of
            ShowProfile -> "Your profile"
            ShowSettings -> "Your settings"
            ShowLeaderboard -> "Karma leaderboard"
            ShowHelp -> "Help"
            ShowExperiments -> "Chat experiments"
            ShowBacker -> "Backing"
            _ -> ""
instance showMDateTime :: Show DateTimeWrapper where
      show = DGRS.genericShow
instance showMDate :: Show DateWrapper where
      show = DGRS.genericShow
instance showMessageContent :: Show MessageContent where
      show = DGRS.genericShow
instance showWebSocketPayloadClient :: Show WebSocketPayloadClient where
      show = DGRS.genericShow
instance showPayloadErrorContext :: Show DatabaseError where
      show = DGRS.genericShow
instance showWebSocketPayloadServer :: Show WebSocketPayloadServer where
      show = DGRS.genericShow

instance showElementID :: Show ElementID where
      show = case _ of
            UserContextMenu -> "user-context-menu"
            SuggestionContextMenu -> "suggestion-context-menu"
            CompactProfileContextMenu -> "compact-profile-context-menu"
            FullProfileContextMenu -> "full-profile-context-menu"
            ImageFileInput -> "image-file-input"
            ContactList -> "contact-list"
            LinkFormUrl -> "link-form-url"
            ChatInput -> "chat-input"
            ChatInputSuggestion -> "chat-input-suggestion"
            ImageFormCaption -> "image-form-caption"
            MessageHistory -> "message-history"
            Favicon -> "favicon"
            ConfirmPasswordInput -> "#confirm-password-input"
            PasswordDiv -> "password"
            TermsLink -> "terms-link"
            PrivacyLink -> "privacy-link"
            Faq -> "faq"
            TermsSection -> "terms"
            EmailDiv -> "email"
            EmailInput -> "email-input"
            PrivacySection -> "privacy"
            ConfirmPassword -> "confirm-password"
            FaqLink -> "faq-link"
            BackerRoot -> "backer-root"
            ChatInputPreview -> "chat-input-preview"
            ProfileEditionRoot -> "profile-edition-root"
            SettingsEditionRoot -> "settings-edition-root"
            KarmaLeaderboard -> "karma-leaderboard-root"
            HelpRoot -> "help-root"
            ExperimentsRoot -> "experiments-root"
            PasswordInput -> "password-input"
            AvatarFileInput -> "avatar-file-input"

instance encodeQueryParamMDateTime :: EncodeQueryParam DateTimeWrapper where
      encodeQueryParam = Just <<< show <<< SDT.dateTimeToNumber



instance readForeignMDatee :: ReadForeign DateWrapper where
      readImpl foreignDate = DateWrapper <<< DTT.date <<<  DDI.toDateTime <<< SU.fromJust <<< DDI.instant <<< DTD.Milliseconds <$> F.readNumber foreignDate
instance readForeignMDateTime :: ReadForeign DateTimeWrapper where
      readImpl foreignDateTime = DateTimeWrapper <<< DDI.toDateTime <<< SU.fromJust <<< DDI.instant <<< DTD.Milliseconds <$> F.readNumber foreignDateTime

instance decodeQueryMDateTime :: DecodeQueryParam DateTimeWrapper where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing -> Left $ QueryParamNotFound { key, queryObj: query }
                  Just [value] -> DM.maybe (errorDecoding query key) (Right <<< DateTimeWrapper <<< DDI.toDateTime) (DDI.instant <<< DTD.Milliseconds =<< DNM.fromString value)
                  _ -> errorDecoding query key

instance writeForeignMDateTime :: WriteForeign DateTimeWrapper where
      writeImpl = F.unsafeToForeign <<< SDT.dateTimeToNumber

instance writeForeignMDate :: WriteForeign DateWrapper where
      writeImpl = F.unsafeToForeign <<< SDT.dateToNumber


derive instance newtypeMDateTime :: Newtype DateTimeWrapper _
derive instance newtypeMDate :: Newtype DateWrapper _
derive instance eqIMSelector :: Eq ElementID
derive instance eqShowContextMenu :: Eq ShowContextMenu
derive instance eqDatabaseError :: Eq DatabaseError
derive instance eqFullContactProfile :: Eq ProfilePresentation
derive instance eqRetryableRequest :: Eq RetryableRequest
derive instance eqShowChatModal :: Eq ShowChatModal
derive instance eqMDateTime :: Eq DateTimeWrapper
derive instance eqMDate :: Eq DateWrapper


derive instance eqShowModal :: Eq ShowUserMenuModal


derive instance genericResponseError :: Generic ResponseError _
derive instance genericMDateTime :: Generic DateTimeWrapper _
derive instance genericMDate :: Generic DateWrapper _
derive instance genericMessageContent :: Generic MessageContent _
derive instance genericWebSocketPayloadServer :: Generic WebSocketPayloadClient _
derive instance genericFullWebSocketPayloadServer :: Generic FullWebSocketPayloadClient _
derive instance genericWebSocketPayloadClient :: Generic WebSocketPayloadServer _
derive instance genericShowModal :: Generic ShowUserMenuModal _


derive instance genericShowContextMenu :: Generic ShowContextMenu _
derive instance genericDatabaseError :: Generic DatabaseError _
derive instance genericRetryableRequest :: Generic RetryableRequest _
derive instance genericShowChatModal :: Generic ShowChatModal _



errorDecoding :: forall a. Object (Array String) -> String -> Either DecodeError a
errorDecoding queryObj key = Left $ QueryDecodeError {
      values: [],
      message: "Could not decode parameter " <> key,
      key,
      queryObj
}