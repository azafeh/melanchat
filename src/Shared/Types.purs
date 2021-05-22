module Shared.Types where

import Prelude

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

foreign import data Trie :: Type

type NoBody = {}


type EmailCaptcha r = {
      email:: String,
      captchaResponse:: Maybe String |
      r
}

-- | Fields for registration or login
type RegisterLogin = (EmailCaptcha (password :: String))

newtype RegisterLoginUser = RegisterLoginUser {
      id :: Int,
      email :: String,
      password :: String
}

type RecoverAccount = EmailCaptcha ()

newtype DateWrapper = DateWrapper Date

type ResetPassword =  {
      token :: String,
      password :: String
}

data Generate =
      Name |
      Headline |
      Description

data By =
      ID Int |
      Email String

-- | Errors that should be reported back to the user
data ResponseError =
      BadRequest { reason :: String } |
      InternalError { reason :: String, context :: Maybe DatabaseError } |
      ExpiredSession

type Report = {
      reason :: ReportReason,
      comment :: Maybe String,
      userID :: Int
}

type InternalHelpModel = {
      toggleHelp :: DisplayHelpSection
}

data DisplayHelpSection =
      FAQ |
      Terms |
      Privacy

data InternalHelpMessage =
      ToggleHelpSection DisplayHelpSection

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

data MountPoint = IM | Profile

type PU = (BasicUser (
      gender :: Maybe Gender,
      country :: Maybe Int,
      languages :: Array Int,
      age :: Maybe DateWrapper
))

type ProfileUser = Record PU

type Choice = Maybe

type PM = (
      user :: ProfileUser,
      nameInputed :: Maybe String,
      headlineInputed :: Maybe String,
      ageInputed :: Choice (Maybe DateWrapper),
      genderInputed :: Choice (Maybe Gender),
      countryInputed :: Choice (Maybe Int),
      languagesInputed :: Maybe Int,
      languagesInputedList :: Maybe (Array Int),
      tagsInputed :: Maybe String,
      tagsInputedList :: Maybe (Array String),
      descriptionInputed :: Maybe String,
      generating :: Maybe Generate,
      countries :: Array (Tuple Int String),
      languages :: Array (Tuple Int String),
      hideSuccessMessage :: Boolean,
      experimenting :: Maybe ExperimentData
)

--used to generically set records
type ProfileModel = Record PM

newtype ProfileUserWrapper = ProfileUserWrapper ProfileUser

data ProfileMessage =
      SetPField (ProfileModel -> ProfileModel) |
      SelectAvatar |
      SetAvatar String |
      SetGenerate Generate |
      SetProfileChatExperiment (Maybe ExperimentData) |
      SaveProfile

type SM = (
      email :: String,
      emailConfirmation :: String,
      password :: String,
      erroredFields :: Array String,
      passwordConfirmation :: String,
      confirmTermination :: Boolean
)

type SettingsModel = Record SM

data SettingsMessage =
      SetSField (SettingsModel -> SettingsModel) |
      ChangeEmail |
      ChangePassword |
      ToggleTerminateAccount |
      TerminateAccount --very bad

data ContentType = JSON | JS | GIF | JPEG | PNG | CSS | HTML | OctetStream

newtype ArrayPrimaryKey = ArrayPrimaryKey (Array Int)

derive instance genericReportReason :: Generic ReportReason _


derive instance genericShowContextMenu :: Generic ShowContextMenu _
derive instance genericDatabaseError :: Generic DatabaseError _
derive instance genericRetryableRequest :: Generic RetryableRequest _
derive instance genericShowChatModal :: Generic ShowChatModal _
derive instance genericDisplayHelpSection :: Generic DisplayHelpSection _

derive instance genericMessageStatus :: Generic MessageStatus _
derive instance genericGenerate :: Generic Generate _
derive instance genericGender :: Generic Gender _
derive instance genericResponseError :: Generic ResponseError _
derive instance genericUser :: Generic RegisterLoginUser _
derive instance genericMDateTime :: Generic DateTimeWrapper _
derive instance genericMDate :: Generic DateWrapper _
derive instance genericMessageContent :: Generic MessageContent _
derive instance genericWebSocketPayloadServer :: Generic WebSocketPayloadClient _
derive instance genericFullWebSocketPayloadServer :: Generic FullWebSocketPayloadClient _
derive instance genericWebSocketPayloadClient :: Generic WebSocketPayloadServer _
derive instance genericShowModal :: Generic ShowUserMenuModal _

derive instance newtypeMessageIDTemporaryWrapper :: Newtype MessageIDTemporaryWrapper _
derive instance newtypeProfileUserWrapper :: Newtype ProfileUserWrapper _
derive instance newtypeMDateTime :: Newtype DateTimeWrapper _
derive instance newtypeMDate :: Newtype DateWrapper _
derive instance newTypeIMUserWrapper :: Newtype IMUserWrapper _
derive instance newTypeContactWrapper :: Newtype ContactWrapper _
derive instance newTypeHistoryMessageWrapper :: Newtype HistoryMessageWrapper _

derive instance eqReportReason :: Eq ReportReason

derive instance eqIMSelector :: Eq ElementID
derive instance eqShowContextMenu :: Eq ShowContextMenu
derive instance eqDatabaseError :: Eq DatabaseError
derive instance eqFullContactProfile :: Eq ProfilePresentation
derive instance eqRetryableRequest :: Eq RetryableRequest
derive instance eqGenerate :: Eq Generate
derive instance eqShowChatModal :: Eq ShowChatModal
derive instance eqDisplayHelpSection :: Eq DisplayHelpSection
derive instance eqMDateTime :: Eq DateTimeWrapper
derive instance eqMDate :: Eq DateWrapper

derive instance eqGender :: Eq Gender
derive instance eqMessageStatus :: Eq MessageStatus
derive instance eqShowModal :: Eq ShowUserMenuModal

-- instance fromSQLRowMessageIDTemporaryWrapper :: FromSQLRow MessageIDTemporaryWrapper where
--       fromSQLRow =
--             case _ of
--                   [foreignID, foreignTemporaryID] -> DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
--                         id <- F.readInt foreignID
--                         temporaryID <- F.readInt foreignTemporaryID
--                         pure $ MessageIDTemporaryWrapper { id, temporaryID }
--                   _ -> Left "missing or extra fields for karma user"

-- instance fromSQLRowExperimentsWrapper :: FromSQLRow ChatExperimentWrapper where
--       fromSQLRow =
--             case _ of
--                   [foreignID, foreignCode, foreignName, foreignDescription] -> DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
--                         id <- F.readInt foreignID
--                         code <- (SU.fromJust <<< DE.toEnum) <$> F.readInt foreignCode
--                         name <- F.readString foreignName
--                         description <- F.readString foreignDescription
--                         pure $ ChatExperimentWrapper {
--                               id,
--                               code,
--                               name,
--                               description
--                         }
--                   _ -> Left "missing or extra fields for experiments"

-- instance fromSQLRowProfileUserWrapper :: FromSQLRow ProfileUserWrapper where
--       fromSQLRow [
--             foreignID,
--             foreignAvatar,
--             foreignGender,
--             foreignBirthday,
--             foreignUnread,
--             foreignHeadline,
--             foreignDescription,
--             foreignCountry,
--             foreignLanguages,
--             foreignTags,
--             foreignKarma,
--             foreignKarmaPosition
--       ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
--             id <- F.readInt foreignID
--             --REFACTOR: all image paths
--             avatar <- readAvatar foreignAvatar
--             name <- F.readString foreignUnread
--             maybeForeignBirthday <- F.readNull foreignBirthday
--             age <- DM.maybe (pure Nothing) (map (Just <<< DTT.date) <<< SDT.readDate) maybeForeignBirthday
--             maybeGender <- F.readNull foreignGender
--             gender <- DM.maybe (pure Nothing) (map DSR.read <<< F.readString) maybeGender
--             headline <- F.readString foreignHeadline
--             description <- F.readString foreignDescription
--             maybeCountry <- F.readNull foreignCountry
--             country <- DM.maybe (pure Nothing) (map Just <<< F.readInt) maybeCountry
--             maybeLanguages :: Maybe Foreign <- F.readNull foreignLanguages
--             foreignIDLanguages <- DM.maybe (pure []) F.readArray maybeLanguages
--             languages <- DT.traverse F.readInt  foreignIDLanguages
--             karma <- F.readInt foreignKarma
--             tags <- readTags foreignTags
--             karmaPosition <- F.readInt foreignKarmaPosition
--             pure $ ProfileUserWrapper {
--                   id,
--                   avatar,
--                   name,
--                   age: DateWrapper <$> age,
--                   gender,
--                   headline,
--                   description,
--                   country,
--                   karma,
--                   languages,
--                   tags,
--                   karmaPosition
--             }
--       fromSQLRow _ = Left "missing or extra fields from users table"

--as it is right now, every query must have a FromSQLRow instance
-- is there not an easier way to do this?

-- instance fromSQLRowResiterLoginUser :: FromSQLRow RegisterLoginUser where
--       fromSQLRow [foreignID, foreignEmail, foreignPassword] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
--             id <- F.readInt foreignID
--             email <- F.readString foreignEmail
--             password <- F.readString foreignPassword
--             pure $ RegisterLoginUser { id, email, password }
--       fromSQLRow _ = Left "missing/extra fields from users table"

-- instance fromSQLRowIMUserWrapper :: FromSQLRow IMUserWrapper where
--       fromSQLRow = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept <<< parseIMUserWrapper

-- instance fromSQLRowContact :: FromSQLRow ContactWrapper where
--       fromSQLRow [
--             _,
--             foreignSender,
--             chatAge,
--             foreignID,
--             foreignAvatar,
--             foreignGender,
--             foreignAge,
--             foreignName,
--             foreignHeadline,
--             foreignDescription,
--             foreignCountry,
--             foreignLanguages,
--             foreignTags,
--             foreignKarma,
--             foreignKarmaPosition
--       ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
--             sender <- F.readInt foreignSender
--             chatAge <- F.readNumber chatAge
--             IMUserWrapper user <- parseIMUserWrapper [
--                   foreignID,
--                   foreignAvatar,
--                   foreignGender,
--                   foreignAge,
--                   foreignName,
--                   foreignHeadline,
--                   foreignDescription,
--                   foreignCountry,
--                   foreignLanguages,
--                   foreignTags,
--                   foreignKarma,
--                   foreignKarmaPosition
--             ]
--             pure $ ContactWrapper {
--                   available: true,
--                   shouldFetchChatHistory: true,
--                   history: [],
--                   impersonating: Nothing,
--                   chatStarter: sender,
--                   chatAge,
--                   user
--             }
--       fromSQLRow _ = Left "missing or extra fields from users table contact projection"

parseIMUserWrapper :: Array Foreign -> F IMUserWrapper
parseIMUserWrapper =
      case _ of
      [     foreignID,
            foreignAvatar,
            foreignGender,
            foreignAge,
            foreignName,
            foreignHeadline,
            foreignDescription,
            foreignCountry,
            foreignLanguages,
            foreignTags,
            foreignKarma,
            foreignKarmaPosition
      ] -> do
            id <- F.readInt foreignID
            avatar <- readAvatar foreignAvatar
            name <- F.readString foreignName
            age <- readAge foreignAge
            maybeGender <- F.readNull foreignGender
            gender <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeGender
            headline <- F.readString foreignHeadline
            description <- F.readString foreignDescription
            maybeCountry <- F.readNull foreignCountry
            karma <- F.readInt foreignKarma
            karmaPosition <- F.readInt foreignKarmaPosition
            country <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeCountry
            maybeLanguages <- F.readNull foreignLanguages
            languages <- DM.maybe (pure []) (map (DS.split (Pattern ",")) <<< F.readString) maybeLanguages
            tags <- readTags foreignTags
            pure $ IMUserWrapper {
                  id,
                  avatar,
                  name,
                  age,
                  gender,
                  headline,
                  description,
                  karma,
                  country,
                  languages,
                  tags,
                  karmaPosition
            }
      _ ->  CME.throwError <<< DLN.singleton $ ForeignError "missing or extra fields from users table imuser projection"


readAge :: Foreign -> F (Maybe Int)
readAge foreignAge = do
      maybeForeignAge <- F.readNull foreignAge
      DM.maybe (pure Nothing) (map Just <<< F.readInt) maybeForeignAge

readAvatar :: Foreign -> F (Maybe String)
readAvatar foreignAvatar = do
      maybeForeignAvatar <- F.readNull foreignAvatar
      DM.maybe (pure Nothing) (map (Just <<< ((imageBasePath <> _) <<< ("upload/" <> _ ))) <<< F.readString) maybeForeignAvatar

--REFACTOR: just use pg arrays for tags and languages
readTags foreignTags = do
      maybeTags <- F.readNull foreignTags
      DM.maybe (pure []) (map (DS.split (Pattern "\\n")) <<< F.readString) maybeTags

-- instance messageWrapperRowFromSQLRow :: FromSQLRow HistoryMessageWrapper where
--       fromSQLRow [
--           foreignID,
--           foreignSender,
--           foreignRecipient,
--           foreignDate,
--           foreignContent,
--           foreignStatus
--       ] = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept $ do
--           id <- F.readInt foreignID
--           sender <- F.readInt foreignSender
--           recipient <- F.readInt foreignRecipient
--           date <- DateTimeWrapper <$> SDT.readDate foreignDate
--           content <- F.readString foreignContent
--           status <- SU.fromJust <<< DE.toEnum <$> F.readInt foreignStatus
--           pure $ HistoryMessageWrapper { id, sender, recipient, date, content, status }
--       fromSQLRow _ = Left "missing or extra fields from users table"

--there is nothing simple about using purescript-simple-json with types other than record
instance writeForeignMDateTime :: WriteForeign DateTimeWrapper where
      writeImpl = F.unsafeToForeign <<< SDT.dateTimeToNumber
instance writeForeignMessageStatus :: WriteForeign MessageStatus where
      writeImpl messageStatus = F.unsafeToForeign $ DE.fromEnum messageStatus
instance writeForeignGender :: WriteForeign Gender where
      writeImpl gender = F.unsafeToForeign $ show gender
instance writeForeignReportReason :: WriteForeign ReportReason where
      writeImpl reason = F.unsafeToForeign $ DE.fromEnum reason
instance writeForeignMDate :: WriteForeign DateWrapper where
      writeImpl = F.unsafeToForeign <<< SDT.dateToNumber

instance readForeignMDatee :: ReadForeign DateWrapper where
      readImpl foreignDate = DateWrapper <<< DTT.date <<<  DDI.toDateTime <<< SU.fromJust <<< DDI.instant <<< DTD.Milliseconds <$> F.readNumber foreignDate
instance readForeignMDateTime :: ReadForeign DateTimeWrapper where
      readImpl foreignDateTime = DateTimeWrapper <<< DDI.toDateTime <<< SU.fromJust <<< DDI.instant <<< DTD.Milliseconds <$> F.readNumber foreignDateTime
--refactor: gender should use enum instances for read/writeforeigner
instance readForeignGender :: ReadForeign Gender where
      readImpl foreignGender = SU.fromJust <<< DSR.read <$> F.readString foreignGender
instance readForeignMessageStatus :: ReadForeign MessageStatus where
      readImpl value = SU.fromJust <<< DE.toEnum <$> F.readInt value
instance readForeignReportReason :: ReadForeign ReportReason where
      readImpl value = SU.fromJust <<< DE.toEnum <$> F.readInt value

instance decodeQueryArrayPrimaryKey :: DecodeQueryParam ArrayPrimaryKey where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing -> Left $ QueryParamNotFound { key, queryObj: query }
                  --this is terrible
                  Just [value] -> Right <<< ArrayPrimaryKey <<< DA.catMaybes <<< map DI.fromString $ DSRG.split (DSRU.unsafeRegex "\\D" noFlags) value
                  _ -> errorDecoding query key
instance decodeQueryGenerate :: DecodeQueryParam Generate where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing -> Left $ QueryParamNotFound { key, queryObj: query }
                  Just [value] -> DM.maybe (errorDecoding query key) Right $ DSR.read value
                  _ -> errorDecoding query key
instance decodeQueryMDateTime :: DecodeQueryParam DateTimeWrapper where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing -> Left $ QueryParamNotFound { key, queryObj: query }
                  Just [value] -> DM.maybe (errorDecoding query key) (Right <<< DateTimeWrapper <<< DDI.toDateTime) (DDI.instant <<< DTD.Milliseconds =<< DNM.fromString value)
                  _ -> errorDecoding query key

errorDecoding :: forall a. Object (Array String) -> String -> Either DecodeError a
errorDecoding queryObj key = Left $ QueryDecodeError {
      values: [],
      message: "Could not decode parameter " <> key,
      key,
      queryObj
}

instance encodeQueryArrayPrimaryKey :: EncodeQueryParam ArrayPrimaryKey where
      encodeQueryParam (ArrayPrimaryKey ap) = Just $ show ap
instance encodeQueryParamMDateTime :: EncodeQueryParam DateTimeWrapper where
      encodeQueryParam = Just <<< show <<< SDT.dateTimeToNumber
instance encodeQueryGenerate :: EncodeQueryParam Generate where
      encodeQueryParam = Just <<< show

instance contentMountPoint :: Show MountPoint where
      show = case _ of
            IM -> "im-mount"
            Profile -> "profile-mount"

instance contentReportReason :: Show ReportReason where
      show = case _ of
            DatingContent -> "Dating content"
            Harrassment -> "Harrassment/Bullying"
            HateSpeech -> "Hate Speech/Call to violence"
            Spam -> "Spam/Product placement"
            OtherReason -> "Other"
instance contentTypeShow :: Show ContentType where
      show JSON = "application/json"
      show JS = "application/javascript"
      show GIF = "image/gif"
      show JPEG = "image/jpeg"
      show PNG = "image/png"
      show CSS = "text/css"
      show HTML = "text/html"
      show _ = "application/octet-stream"
instance showGenerate :: Show Generate where
      show = DGRS.genericShow
instance showMessageStatus :: Show MessageStatus where
      show = case _ of
            Errored -> "Failed to send"
            Sent -> "Sending"
            Received -> "Sent"
            Delivered -> "Unread"
            Read -> "Read"

instance showResponseError :: Show ResponseError where
      show = DGRS.genericShow
instance showGender :: Show Gender where
      show Female = "Female"
      show Male = "Male"
      show NonBinary = "Non binary"
      show Other = "Other"
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

-- instance toSQLValueGender :: ToSQLValue Gender where
--       toSQLValue = F.unsafeToForeign <<< show
-- instance toSQLValueMessageStatus :: ToSQLValue MessageStatus where
--       toSQLValue = F.unsafeToForeign <<< DE.fromEnum
-- instance toSQLReportReason :: ToSQLValue ReportReason where
--       toSQLValue = F.unsafeToForeign <<< DE.fromEnum

-- instance fromSQLValueGender :: FromSQLValue Gender where
--       fromSQLValue = DB.lmap show <<< CME.runExcept <<< map (SU.fromJust <<< DSR.read) <<< F.readString

instance hashableIMSelector :: Hashable ElementID where
      hash = HS.hash <<< show

instance encodeJsonReportReason :: EncodeJson ReportReason where
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
instance encodeJsonGenerate :: EncodeJson Generate where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonDisplayHelpSection :: EncodeJson DisplayHelpSection where
      encodeJson = DAEGR.genericEncodeJson

instance encodeJsonMessageStatus :: EncodeJson MessageStatus where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonGender :: EncodeJson Gender where
      encodeJson = DAEGR.genericEncodeJson
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

instance decodeJsonReportReason :: DecodeJson ReportReason  where
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
instance decodeJsonGenerate :: DecodeJson Generate where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonDisplayHelpSection :: DecodeJson DisplayHelpSection where
      decodeJson = DADGR.genericDecodeJson

instance decodeJsonMessageStatus :: DecodeJson MessageStatus where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonGender :: DecodeJson Gender where
      decodeJson = DADGR.genericDecodeJson
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

-- match both on file extension or content type
instance contentTypeRead :: Read ContentType where
      read v =
            Just $
                  if value == ".json" || value == show JSON then JSON
                   else if value == ".js" || value == show JS then JS
                   else if value == ".gif" || value == show GIF then GIF
                   else if value == ".jpeg" || value == ".jpg" || value == show JPEG then JPEG
                   else if value == ".png" || value == show PNG then PNG
                   else if value == ".css" || value == show CSS then CSS
                   else if value == ".html" || value == show HTML then HTML
                   else OctetStream
            where value = DS.trim $ DS.toLower v


instance readGender :: Read Gender where
      read input =
          case DS.toLower $ DS.trim input of
              "female" -> Just Female
              "male" -> Just Male
              "non binary" -> Just NonBinary
              "other" -> Just Other
              _ -> Nothing

instance readGenerate :: Read Generate where
      read input =
          case DS.toLower $ DS.trim input of
              "name" -> Just Name
              "headline" -> Just Headline
              "description" -> Just Description
              _ -> Nothing

--thats a lot of work...

derive instance ordReportReason :: Ord ReportReason
derive instance ordMessageStatus :: Ord MessageStatus

instance boundedReportReason :: Bounded ReportReason where
      bottom = DatingContent
      top = OtherReason
instance boundedMessageStatus :: Bounded MessageStatus where
      bottom = Received
      top = Read

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
