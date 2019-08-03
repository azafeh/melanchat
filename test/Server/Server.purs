module Test.Server where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Database.PostgreSQL (Pool, Query(..), Row0(..))
import Database.PostgreSQL as DP
import Effect.Aff (Aff)
import Run (Run, AFF, EFFECT)
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Run.State as RS
import Server.Database as SD

configuration :: Configuration
configuration = Configuration {
        port: 8000,
        development: true,
        captchaSecret: "",
        benderURL: "",
        useBender: false,
        tokenSecretGET: "abc",
        tokenSecretPOST: "def",
        salt: "ghi"
}

newTestPool ∷ Aff Pool
newTestPool = DP.newPool $ (DP.defaultPoolConfiguration "melanchatTest") {
        user = Just "melanchat",
        idleTimeoutMillis = Just 1000
}

serverAction :: (Unit -> ServerEffect Unit) -> Aff Unit
serverAction action = do
        pool <- newTestPool
        R.runBaseAff' <<<
        RE.catch (const (pure unit)) <<<
        RS.evalState {
                session : { user : Nothing }
        } <<<
        RR.runReader {
                configuration,
                pool
        } $ do
                truncateTables
                action unit

serverActionCatch :: (ResponseError -> Run (aff :: AFF, effect :: EFFECT) Unit) -> (Unit -> ServerEffect Unit) -> Aff Unit
serverActionCatch catch action  = do
        pool <- newTestPool
        R.runBaseAff' <<<
        RE.catch catch <<<
        RS.evalState {
                session : { user : Nothing }
        } <<<
        RR.runReader {
                configuration,
                pool
        } $ do
                truncateTables
                action unit

truncateTables :: ServerEffect Unit
truncateTables = SD.execute (Query "select truncateTables()") Row0
