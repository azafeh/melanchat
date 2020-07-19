module Server.Email where

import Prelude

import Effect.Uncurried (EffectFn3)
import Effect.Uncurried as EU
import Run as R
import Run.Reader as RR
import Server.Types (Configuration(..), ServerEffect)

foreign import sendEmail_ :: EffectFn3 { host :: String, user :: String, password :: String } String String Unit

sendEmail :: String -> String -> ServerEffect Unit
sendEmail to content = do
        {configuration: Configuration { development, emailUser, emailHost, emailPassword }} <- RR.ask
        unless development <<< R.liftEffect $ EU.runEffectFn3 sendEmail_ { user: emailUser, host: emailHost, password: emailPassword } to content

