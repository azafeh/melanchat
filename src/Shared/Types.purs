module Shared.Types where

--refactor: split into apt modules

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
import Droplet.Language (class ToValue, class FromValue)
import Droplet.Language as DL
import Shared.IM.Types
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

type RegisterLoginUser = {
      id :: Int,
      email :: String,
      password :: String
}

type RecoverAccount = EmailCaptcha ()



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
      countries :: Array {id :: Int, name :: String},
      languages :: Array {id :: Int, name :: String},
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

--should be in user
data Gender =
      Female |
      Male |
      NonBinary |
      Other

newtype ArrayPrimaryKey = ArrayPrimaryKey (Array Int)

derive instance eqGender :: Eq Gender


derive instance genericGenerate :: Generic Generate _

derive instance eqGenerate :: Eq Generate


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

-- instance fromSQLRowIMUserWrapper :: FromSQLRow IMUser where
--       fromSQLRow = DB.lmap (DLN.foldMap F.renderForeignError) <<< CME.runExcept <<< parseIMUserWrapper

-- instance fromSQLRowContact :: FromSQLRow Contact where
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
--             IMUser user <- parseIMUserWrapper [
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
--             pure $ Contact {
--                   available: true,
--                   shouldFetchChatHistory: true,
--                   history: [],
--                   impersonating: Nothing,
--                   chatStarter: sender,
--                   chatAge,
--                   user
--             }
--       fromSQLRow _ = Left "missing or extra fields from users table contact projection"

-- parseIMUserWrapper :: Array Foreign -> F IMUser
-- parseIMUserWrapper =
--       case _ of
--       [     foreignID,
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
--       ] -> do
--             id <- F.readInt foreignID
--             avatar <- readAvatar foreignAvatar
--             name <- F.readString foreignName
--             age <- readAge foreignAge
--             maybeGender <- F.readNull foreignGender
--             gender <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeGender
--             headline <- F.readString foreignHeadline
--             description <- F.readString foreignDescription
--             maybeCountry <- F.readNull foreignCountry
--             karma <- F.readInt foreignKarma
--             karmaPosition <- F.readInt foreignKarmaPosition
--             country <- DM.maybe (pure Nothing) (map Just <<< F.readString) maybeCountry
--             maybeLanguages <- F.readNull foreignLanguages
--             languages <- DM.maybe (pure []) (map (DS.split (Pattern ",")) <<< F.readString) maybeLanguages
--             tags <- readTags foreignTags
--             pure $ IMUser {
--                   id,
--                   avatar,
--                   name,
--                   age,
--                   gender,
--                   headline,
--                   description,
--                   karma,
--                   country,
--                   languages,
--                   tags,
--                   karmaPosition
--             }
--       _ ->  CME.throwError <<< DLN.singleton $ ForeignError "missing or extra fields from users table imuser projection"


-- readAge :: Foreign -> F (Maybe Int)
-- readAge foreignAge = do
--       maybeForeignAge <- F.readNull foreignAge
--       DM.maybe (pure Nothing) (map Just <<< F.readInt) maybeForeignAge

-- readAvatar :: Foreign -> F (Maybe String)
-- readAvatar foreignAvatar = do
--       maybeForeignAvatar <- F.readNull foreignAvatar
--       DM.maybe (pure Nothing) (map (Just <<< ((imageBasePath <> _) <<< ("upload/" <> _ ))) <<< F.readString) maybeForeignAvatar

-- --REFACTOR: just use pg arrays for tags and languages
-- readTags foreignTags = do
--       maybeTags <- F.readNull foreignTags
--       DM.maybe (pure []) (map (DS.split (Pattern "\\n")) <<< F.readString) maybeTags

-- instance messageWrapperRowFromSQLRow :: FromSQLRow HistoryMessage where
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
--           pure $ HistoryMessage { id, sender, recipient, date, content, status }
--       fromSQLRow _ = Left "missing or extra fields from users table"

--there is nothing simple about using purescript-simple-json with types other than record



instance contentMountPoint :: Show MountPoint where
      show = case _ of
            IM -> "im-mount"
            Profile -> "profile-mount"

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

instance decodeQueryGenerate :: DecodeQueryParam Generate where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing -> Left $ QueryParamNotFound { key, queryObj: query }
                  Just [value] -> DM.maybe (errorDecoding query key) Right $ DSR.read value
                  _ -> errorDecoding query key




instance decodeQueryArrayPrimaryKey :: DecodeQueryParam ArrayPrimaryKey where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing -> Left $ QueryParamNotFound { key, queryObj: query }
                  --this is terrible
                  Just [value] -> Right <<< ArrayPrimaryKey <<< DA.catMaybes <<< map DI.fromString $ DSRG.split (DSRU.unsafeRegex "\\D" noFlags) value
                  _ -> errorDecoding query key


errorDecoding :: forall a. Object (Array String) -> String -> Either DecodeError a
errorDecoding queryObj key = Left $ QueryDecodeError {
      values: [],
      message: "Could not decode parameter " <> key,
      key,
      queryObj
}


instance encodeQueryGenerate :: EncodeQueryParam Generate where
      encodeQueryParam = Just <<< show


instance decodeJsonGender :: DecodeJson Gender where
      decodeJson = DADGR.genericDecodeJson


instance encodeJsonGender :: EncodeJson Gender where
      encodeJson = DAEGR.genericEncodeJson

instance showGender :: Show Gender where
      show Female = "Female"
      show Male = "Male"
      show NonBinary = "Non binary"
      show Other = "Other"

instance readForeignGender :: ReadForeign Gender where
      readImpl foreignGender = SU.fromJust <<< DSR.read <$> F.readString foreignGender

derive instance genericDisplayHelpSection :: Generic DisplayHelpSection _
derive instance genericGender :: Generic Gender _

instance writeForeignGender :: WriteForeign Gender where
      writeImpl gender = F.unsafeToForeign $ show gender


instance genderToValue :: ToValue Gender where
      toValue = F.unsafeToForeign <<< DE.fromEnum

instance encodeQueryArrayPrimaryKey :: EncodeQueryParam ArrayPrimaryKey where
      encodeQueryParam (ArrayPrimaryKey ap) = Just $ show ap

instance encodeJsonGenerate :: EncodeJson Generate where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonDisplayHelpSection :: EncodeJson DisplayHelpSection where
      encodeJson = DAEGR.genericEncodeJson


derive instance eqDisplayHelpSection :: Eq DisplayHelpSection


instance decodeJsonGenerate :: DecodeJson Generate where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonDisplayHelpSection :: DecodeJson DisplayHelpSection where
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

derive instance ooGender :: Ord Gender

--should just use Enum
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

instance oGender :: Bounded Gender where
      bottom = Female
      top = Other

instance bGender :: BoundedEnum Gender where
      cardinality = Cardinality 1

      fromEnum = case _ of
            Female -> 0
            Male -> 1
            NonBinary -> 2
            Other -> 3

      toEnum = case _ of
            0 -> Just Female
            1 -> Just Male
            2 -> Just NonBinary
            3 -> Just Other
            _ -> Nothing

instance gEnum :: Enum Gender where
      succ = case _ of
            Female -> Just Male
            Male -> Just NonBinary
            NonBinary -> Just Other
            Other -> Nothing

      pred = case _ of
            Female -> Nothing
            Male -> Just Female
            NonBinary -> Just Male
            Other -> Just NonBinary


instance geFromValue :: FromValue Gender where
      fromValue v = map (SU.fromJust <<< DE.toEnum)  (DL.fromValue v :: Either String Int)