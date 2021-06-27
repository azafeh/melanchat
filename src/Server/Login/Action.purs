module Server.Login.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Server.AccountValidation as SA
import Server.Database.Users as SDU
import Server.Response as SR
import Server.Token as ST

invalidLogin :: String
invalidLogin = "Email not registered or incorrect password"

login :: RegisterLogin -> ServerEffect String
login { email: rawEmail, password } = do
      email <- SA.validateEmail rawEmail
      hash <- SA.validatePassword password
      maybeUser <- SDU.userBy $ Email email
      case maybeUser of
            Nothing -> SR.throwBadRequest invalidLogin
            Just user -> do
                  when (hash /= user.password) $ SR.throwBadRequest invalidLogin
                  ST.createToken user.id