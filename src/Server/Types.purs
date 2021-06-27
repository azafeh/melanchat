-- | Types common to all server side modules
module Server.Types where

import Prelude
import Shared.Types

import Control.Monad.Except (ExceptT)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic as DADGR
import Data.Enum (class BoundedEnum, Cardinality(..), class Enum)
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.Maybe (Maybe(..))
import Shared.IM.Types
import Data.Newtype (class Newtype)
import Type.Row (type (+))
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Droplet.Driver(Pool)
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes as PR
import Payload.Server.Response (class EncodeResponse)
import Droplet.Driver(PgError)
import Run (AFF, Run, EFFECT)
import Run.Except (EXCEPT)
import Run.Reader (READER)
import Server.WebSocket (WebSocketConnection, AliveWebSocketConnection)

type ProfileUserEdition = {
      id :: String,
      avatar :: Maybe String,
      name :: String,
      headline :: String,
      description :: String,
      gender :: Maybe Gender,
      country :: Maybe Int,
      tags :: Array String,
      birthday :: Maybe DateWrapper
}

type Configuration = {
      port :: Int,
      captchaSecret :: String,
      tokenSecret :: String,
      salt :: String,
      databaseHost :: Maybe String,
      emailHost :: String,
      emailUser :: String,
      emailPassword :: String,
      randomizeProfiles :: Boolean
}

newtype CaptchaResponse = CaptchaResponse {
      success :: Boolean
}

instance decodeCaptchaResponse :: DecodeJson CaptchaResponse where
      decodeJson json = do
            object <- DAD.decodeJson json
            success <- DAD.getField object "success"
            pure $ CaptchaResponse { success }

type Session = {
      userID :: Maybe Int
}

type BaseReader extension = {
      storageDetails :: Ref StorageDetails,
      pool :: Pool |
      extension
}

type WebSocketReader = BaseReader (
      sessionUserID :: Int,
      connection :: WebSocketConnection,
      allConnections:: Ref (HashMap Int AliveWebSocketConnection)
)

type ServerReader = BaseReader (
      configuration :: Configuration,
      session :: Session
)

type BaseEffect r a = Run (READER r + EXCEPT ResponseError + AFF + EFFECT + ()) a

type DatabaseEffect a = Run (EXCEPT PgError + AFF + ()) a

type ServerEffect a = BaseEffect ServerReader a

type WebSocketEffect = BaseEffect WebSocketReader Unit

type StorageDetails = {
      authenticationKey :: String,
      accountAuthorizationToken :: Maybe String,
      uploadAuthorizationToken :: Maybe String,
      uploadUrl :: Maybe String,
      apiUrl :: Maybe String
}

type AuthorizeAccountResponse =  {
      authorizationToken :: String,
      apiUrl :: String
}

type GetUploadUrlResponse =  {
      authorizationToken :: String,
      uploadUrl :: String
}

data ThreeKAction = Name | Description

instance threeKActionShow :: Show ThreeKAction where
      show Name = "name"
      show Description = "description"

derive instance eqThreeKAction :: Eq ThreeKAction

--thats a lot of work...
instance ordThreeKAction :: Ord ThreeKAction where
      compare Name Description = LT
      compare Description Name = GT
      compare _ _ = EQ

instance boundedThreeKAction :: Bounded ThreeKAction where
      bottom = Name
      top = Description

instance boundedEnumThreeKAction :: BoundedEnum ThreeKAction where
      cardinality = Cardinality 1

      fromEnum Name = 0
      fromEnum Description = 1

      toEnum 0 = Just Name
      toEnum 1 = Just Description
      toEnum _ = Nothing

instance enumThreeKAction :: Enum ThreeKAction where
      succ Name = Just Description
      succ Description = Nothing

      pred Name = Nothing
      pred Description = Just Name

type Ok = Record ()

newtype Html = Html String

derive instance newtypeHtml :: Newtype Html _

instance encodeResponseHtml :: EncodeResponse Html where
      encodeResponse (PR.Response { status, headers, body: Html contents }) = pure $ PR.Response {
            headers: PH.setIfNotDefined "content-type" html headers,
            body: PR.StringBody contents,
            status
      }
