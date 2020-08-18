module Test.Server.Token where

import Prelude
import Server.Types
import Shared.Types

import Data.Int53 as DI
import Run as R
import Run.Reader as RR
import Server.Token as ST
import Shared.Unsafe as SU
import Test.Server as TS
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "token" do
            TU.test "token encoding decoding" do
                  TS.serverAction $ \_ -> do
                        let id =  DI.fromInt 23
                            key = PrimaryKey id
                        { configuration :configuration } <- RR.ask
                        Token { tokenGET, tokenPOST } <- ST.createToken id

                        userIDGET <- SU.fromJust  <$> R.liftEffect (ST.userIDFromToken configuration.tokenSecretGET tokenGET)
                        R.liftAff $ TUA.equal key userIDGET

                        userIDPOST <- SU.fromJust <$> R.liftEffect (ST.userIDFromToken configuration.tokenSecret tokenPOST)
                        R.liftAff $ TUA.equal key userIDPOST