module Server.Database where

import Prelude
import Server.Types
import Shared.Types

import Control.Monad.Except as CME
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Droplet.Driver(PgError(..), Connection, Pool)
import Data.Either as DT
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff
import Effect.Console as EC
import Effect.Exception.Unsafe as EEU
import Shared.IM.Types as SIT
import Droplet.Driver as DD
import Shared.IM.Types
import Droplet.Driver.Unsafe as DDU
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Shared.Types as ST
import Shared.Unsafe as SU

--this makes pg interpret bigints as ints (since we don't use them)
foreign import setUpConversions :: Effect Unit

query q = withConnection $ \connection -> hoist $ DD.query connection q

single q = withConnection $ \connection -> singleWith connection q

singleWith connection q = hoist $ DD.single connection q

execute q = withConnection $ \connection -> executeWith connection q

executeWith connection q = hoistMaybe $ DD.execute connection q

unsafeQuery q parameters = withConnection $ \connection -> unsafeQueryWith connection q parameters

unsafeQueryWith connection q parameters = hoist $ DDU.unsafeQuery connection Nothing q parameters

unsafeSingle q parameters = withConnection $ \connection -> unsafeSingleWith connection q parameters

unsafeSingleWith connection q parameters = hoist $ DDU.unsafeSingle connection Nothing q parameters

unsafeExecute q parameters = withConnection $ \connection -> unsafeExecuteWith connection q parameters

unsafeExecuteWith connection q parameters = hoistMaybe $ DDU.unsafeExecute connection Nothing q parameters

--hoist :: Aff (Either PgError result) -> DatabaseEffect result
hoist action = do
      result <- R.liftAff action
      case result of
            Right r -> pure r
            Left err -> RE.throw err

hoistMaybe action = do
      result <- R.liftAff action
      case result of
            Nothing -> pure unit
            Just err -> RE.throw err

newPool ∷ Configuration -> Effect Pool
newPool { databaseHost } = do
      setUpConversions
      DD.newPool $ (DD.defaultConfiguration "melanchat") {
            user = Just "melanchat",
            host = databaseHost,
            idleTimeoutMillis = Just 1000
      }

withConnection :: forall r result. (Connection -> DatabaseEffect result) -> BaseEffect { pool :: Pool | r } result
withConnection handler = do
      { pool } <- RR.ask
      result <- R.liftAff $ DD.withConnection pool runConnection
      finish result
      where runConnection = R.runBaseAff <<< RE.runExcept <<< case _ of
                  Right c -> handler c
                  Left err -> RE.throw err

            finish = case _ of
                  Right result -> pure result
                  Left error -> throwError error

withTransaction :: forall result r. (Connection -> DatabaseEffect result) -> BaseEffect { pool :: Pool | r } result
withTransaction handler = do
      { pool } <- RR.ask
      result <- R.liftAff (DD.withTransaction pool (R.runBaseAff <<< RE.runExcept <<< handler) :: Aff (Either PgError (Either PgError result)))
      finish result
      where  finish = case _ of
                  Right result -> DT.either throwError pure result
                  Left error -> throwError error

throwError :: forall r error. PgError -> BaseEffect { pool :: Pool | r } error
throwError error = do
      liftEffect $ EC.log errorMessage
      RE.throw $ SIT.InternalError { reason: errorMessage, context: checkRevelanceError error }
      where errorMessage = show error
            checkRevelanceError = case _ of
                  --this is absolutely vile
                  IntegrityError _ -> Just MissingForeignKey
                  _ -> Nothing
