module Server.Guard where

import Prelude
import Server.Types
import Shared.Types

import Data.Either (Either(..))
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Data.Maybe as DMB
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.HTTP (Request)
import Node.HTTP as NH
import Payload.Headers (empty)
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Payload.Server.Guards as PSG
import Payload.Server.Response as PSR
import Server.Cookies (cookieName)
import Server.Token as ST
import Shared.Routes (routes)

guards :: Configuration -> _
guards configuration = {
      loggedUserID: checkLoggedUser configuration,
      checkAnonymous: checkAnonymous configuration
}

checkLoggedUser :: Configuration -> Request -> Aff (Either (Response Empty) Int)
checkLoggedUser { tokenSecret } request = do
      cookies <- PSG.cookies request
      maybeUserID <- liftEffect $ ST.userIDFromToken tokenSecret <<< DMB.fromMaybe "" $ DM.lookup cookieName cookies
      case maybeUserID of
            Just userID -> pure $ Right userID
            _ ->
                  if isPost then
                        pure <<< Left $ PSR.unauthorized Empty
                   else
                        redirectLogin
      where isPost = NH.requestMethod request == "POST"
            redirectLogin = redirect $ routes.login.get { query: {next: Just $ NH.requestURL request} }

checkAnonymous :: Configuration -> Request -> Aff (Either (Response Empty) Unit)
checkAnonymous { tokenSecret } request = do
      cookies <- PSG.cookies request
      maybeUserID <- liftEffect $ ST.userIDFromToken tokenSecret <<< DMB.fromMaybe "" $ DM.lookup cookieName cookies
      case maybeUserID of
            Just userID ->
                  if isPost then
                        pure <<< Left $ PSR.forbidden Empty
                   else
                        redirectIM
            _ -> pure $ Right unit
      where isPost = NH.requestMethod request == "POST"
            redirectIM = redirect $ routes.im.get {}

badRequest :: forall r. Aff (Either (Response Empty) r)
badRequest = pure <<< Left $ PSR.badRequest Empty

redirect :: forall r. String -> Aff (Either (Response Empty) r)
redirect route = pure <<< Left <<< PSR.setHeaders location $ PSR.found Empty
      where location = PH.set "Location" route empty