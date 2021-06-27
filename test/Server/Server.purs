module Test.Server where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as ER
import Run (Run, AFF, EFFECT)
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Server.Configuration as SC
import Server.Database as SD
import Shared.IM.Types
import Test.Server.Model (storageDetails)
import Test.Unit (failure) as TUA
import Droplet.Driver (Pool)
import Droplet.Driver as DD
import Test.Unit as TU
import Test.Unit.Assert (equal) as TUA
import Type.Row (type (+))

session :: Session
session = { userID : Nothing }

newTestPool ∷ Configuration -> Effect Pool
newTestPool { databaseHost }= do
      SD.setUpConversions
      DD.newPool $ (DD.defaultConfiguration "melanchat_test") {
            user = Just "melanchat",
            host = databaseHost,
            idleTimeoutMillis = Just 1000
      }

serverAction :: forall a. ServerEffect a -> Aff Unit
serverAction action = do
      configuration <- liftEffect SC.readConfiguration
      pool <- liftEffect $ newTestPool configuration
      ref <- liftEffect $ ER.new storageDetails
      R.runBaseAff' <<< RE.catch (\ex -> R.liftAff $ TUA.failure ("unexpected exception caught: " <> show ex)) <<< RR.runReader {
            storageDetails: ref,
            configuration,
            pool,
            session
      } $ do
            truncateTables
            void action

serverActionCatch :: forall a. (ResponseError -> Run (AFF + EFFECT + ()) Unit) -> ServerEffect a -> Aff Unit
serverActionCatch catch action  = do
      configuration <- liftEffect SC.readConfiguration
      pool <- liftEffect $ newTestPool configuration
      ref <- liftEffect $ ER.new storageDetails
      R.runBaseAff' <<< RE.catch catch <<< RR.runReader {
            storageDetails: ref,
            configuration,
            pool,
            session
      } $ do
            truncateTables
            void action

truncateTables :: ServerEffect Unit
truncateTables = SD.unsafeExecute "select truncate_tables()" {}

catch expected =
      case _ of
            BadRequest { reason } -> R.liftAff $ TUA.equal expected reason
            other -> R.liftAff <<< TU.failure $ "Unexpected exception: " <> show other
