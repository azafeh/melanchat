--missing tests
module Server.Recover.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.UUID as DU
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Run as R
import Server.AccountValidation as SA
import Server.Captcha as SC
import Server.Database.Users as SDU
import Server.Email as SE
import Server.Ok (ok)
import Server.Recover.Database as SRD
import Server.Response as SR
import Server.Token as ST
import Shared.Options.Domain (domain)
import Shared.Routes (routes)

accountNotFound :: String
accountNotFound = "Could not find an account with this email"

invalidRecovery :: String
invalidRecovery = "Invalid recovery link"

recover :: RecoverAccount -> ServerEffect Ok
recover { email: rawEmail, captchaResponse } = do
      SC.validateCaptcha captchaResponse
      email <- SA.validateEmail rawEmail
      user <- SDU.userBy $ Email email
      case user of
            Nothing -> SR.throwBadRequest accountNotFound
            Just { id } -> do
                  token <- R.liftEffect (DU.toString <$> DU.genUUID)
                  SRD.insertRecover id token
                  contents <- R.liftEffect <<< FRS.render $ HE.html_ [
                        HE.head_ $ HE.title "MelanChat password recovery",
                        HE.body_ [
                              HE.text "Hello!",
                              HE.br,
                              HE.br,
                              HE.a (HA.href $ DS.joinWith "" ["https://", domain, routes.recover.get {query: { token: Just token }}]) "Click here to reset your password.",
                              HE.text " If you didn't ask to recover your password, just ignore this email. Your account will be safe."
                        ]
                  ]
                  SE.sendEmail email "Reset password" contents
                  pure ok

reset :: ResetPassword -> ServerEffect Ok
reset { token, password } = do
      hash <- SA.validatePassword password
      maybeID <- SRD.selectRecoverer token
      case maybeID of
            Nothing -> SR.throwBadRequest invalidRecovery
            Just id -> do
                  hashedPassword <- ST.hashPassword password
                  SRD.recoverPassword token id hashedPassword
                  pure ok
