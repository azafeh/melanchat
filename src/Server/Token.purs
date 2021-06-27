module Server.Token where

import Prelude
import Server.Types
import Shared.Types

import Data.Array.NonEmpty as DAN
import Data.Either as DE

import Data.Maybe (Maybe(..))
import Data.String.Regex as DSR
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe as DSSU
import Effect (Effect)
import Node.Crypto.Hash as NCHA
import Node.Crypto.Hmac as NCH
import Node.Simple.Jwt as NSJ
import Run as R
import Data.Int as DI
import Run.Reader as RR

hashPassword :: String -> ServerEffect String
hashPassword password = do
      { configuration: { salt } } <- RR.ask
      R.liftEffect $ NCH.hex NCHA.SHA512 salt password

createToken :: Int -> ServerEffect String
createToken id = do
      { configuration: { tokenSecret } } <- RR.ask
      NSJ.toString <$> (R.liftEffect <<< NSJ.encode tokenSecret NSJ.HS512 $ show id)

userIDFromToken :: String -> String -> Effect (Maybe Int)
userIDFromToken secret = map (DE.either (const Nothing) DI.fromString) <<< NSJ.decode secret <<< NSJ.fromString