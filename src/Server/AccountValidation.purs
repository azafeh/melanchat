module Server.AccountValidation where

import Prelude
import Server.Types

import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Server.Database.Users as SDU
import Server.Response as SR
import Server.Token as ST
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Types (By(..))

invalidEmailMessage :: String
invalidEmailMessage = "Invalid email"

invalidPasswordMessage :: String
invalidPasswordMessage = "Password must be between " <> show passwordMinCharacters <> " and " <> show passwordMaxCharacters

emailAlreadyRegisteredMessage :: String
emailAlreadyRegisteredMessage = "Email already registered"

validateEmail :: String -> ServerEffect String
validateEmail rawEmail = do
      let email = DS.trim rawEmail
      when (DS.length email > emailMaxCharacters || not (DS.contains (Pattern "@") email) || not (DS.contains (Pattern ".") email)) $ SR.throwBadRequest invalidEmailMessage
      pure email

validateExistingEmail :: String -> ServerEffect Unit
validateExistingEmail email = do
      maybeUser <- SDU.userBy $ Email email
      when (DM.isJust maybeUser) $ SR.throwBadRequest emailAlreadyRegisteredMessage

validatePassword :: String -> ServerEffect String
validatePassword rawPassword = do
      let password = DS.trim rawPassword
      when (DS.length password < passwordMinCharacters || DS.length password > passwordMaxCharacters) $ SR.throwBadRequest invalidPasswordMessage
      hash <- ST.hashPassword password
      pure hash