module Client.Settings.Account where

import Prelude
import Shared.Types

import Client.Common.Location as CCL
import Client.Common.Network (request)
import Client.Common.Network as CNN
import Client.Common.Types (RequestStatus(..))
import Data.Symbol (class IsSymbol)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Type.Proxy(Proxy(..))
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Payload.Client (ClientResponse)
import Shared.Routes (routes)
import Shared.Settings.View as SSV

update :: AffUpdate SettingsModel SettingsMessage
update { model, message } =
      case message of
            SetSField setter -> pure setter
            ChangeEmail -> changeEmail model
            ChangePassword -> changePassword model
            ToggleTerminateAccount -> toggleTerminateAccount model
            TerminateAccount -> terminateAccount

toggleTerminateAccount :: SettingsModel -> Aff (SettingsModel -> SettingsModel)
toggleTerminateAccount _ = pure (\model -> model { confirmTermination = not model.confirmTermination})

changeEmail :: SettingsModel -> Aff (SettingsModel -> SettingsModel)
changeEmail model@({ email, emailConfirmation }) = requestAndLogout (Proxy :: Proxy "email") $ request.settings.account.email { body: { email } }

changePassword :: SettingsModel -> Aff (SettingsModel -> SettingsModel)
changePassword model@({ password, passwordConfirmation }) = requestAndLogout (Proxy :: Proxy "password") $ request.settings.account.password { body: { password } }

requestAndLogout :: forall v field. IsSymbol field => Proxy field -> Aff (ClientResponse v) -> Aff (SettingsModel -> SettingsModel)
requestAndLogout field aff = do
      status <- CNN.formRequest (SSV.formId field) aff
      when (status == Success) $ do
            EA.delay $ Milliseconds 3000.0
            liftEffect <<< CCL.setLocation $ routes.login.get {}
      FAE.noChanges

terminateAccount :: Aff (SettingsModel -> SettingsModel)
terminateAccount = requestAndLogout (Proxy :: Proxy "confirmTermination") $ request.settings.account.terminate { body:{} }
