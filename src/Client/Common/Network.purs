module Client.Common.Network(
        post,
        post'
) where

import Prelude

import Affjax as A
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Client.Common.Notification as CCN
import Client.Common.Storage (tokenKey)
import Client.Common.Storage as CCS
import Control.Monad.Error.Class as CMEC
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Either (Either(..))
import Data.Either as DE
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as EE
import Partial.Unsafe as PU
import Shared.Header (xAccessToken)

-- | A simplified version of post without the option to handle errors
post' :: forall contents c response r. Generic contents c => EncodeRep c => Generic response r => DecodeRep r => String -> contents -> Aff response
post' url data' = do
        response <- post url data'
        case response of
                Right right -> pure right
                Left error -> alertResponseError $ A.printResponseFormatError error

-- | Performs a POST request
post :: forall contents c response r. Generic contents c => EncodeRep c => Generic response r => DecodeRep r => String -> contents -> Aff (Either ResponseFormatError response)
post url data' = do
        --see Token in shared/Types.purs
        token <- liftEffect $ CCS.getItem tokenKey
        request url POST [RequestHeader xAccessToken token] data'

-- | Performs a HTTP request with a JSON payload
request :: forall contents c response r. Generic contents c => EncodeRep c => Generic response r => DecodeRep r => String -> Method -> Array RequestHeader -> contents -> Aff (Either ResponseFormatError response)
request url method extraHeaders data' = do
        response <- A.request $ A.defaultRequest {
                        url = url,
                        method = Left method,
                        responseFormat = RF.json,
                        headers = [
                                Accept $ MediaType "application/json",
                                ContentType $ MediaType "application/json"
                        ] <> extraHeaders,
                        content = Just <<< RB.json $ DAEGR.genericEncodeJson data'
                }
        case response.body of
                Right payload ->
                        if response.status == StatusCode 200 then
                                DE.either alertResponseError (pure <<< Right) $ DADGR.genericDecodeJson payload
                         else
                                alertResponseError <<< PU.unsafePartial $ DE.fromRight $ DAD.decodeJson payload
                Left left -> pure $ Left left

--type this shit
alertResponseError message = do
        liftEffect $ CCN.alert message
        CMEC.throwError <<< EE.error $ "Error: " <> message