module Shared.Experiments.Types where

import Data.Maybe(Maybe(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Show.Generic as DGRS
import Prelude

import Data.Either(Either(..))
import Droplet.Language
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR
import Shared.User

type ImpersonationProfile = Record IU

type ChatExperiment = {
      id :: Int,
      code :: ExperimentData,
      name :: String,
      description :: String
}

data ChatExperimentMessage =
      QuitExperiment |
      JoinExperiment ExperimentData |
      ToggleSection ChatExperimentSection |
      ConfirmImpersonation (Maybe ImpersonationProfile)

data ChatExperimentSection =
      HideSections |
      Characters |
      HistoricalFigures |
      Celebrities

type ChatExperimentModel = {
      experiments :: Array ChatExperiment,
      section :: ChatExperimentSection,
      current :: Maybe ExperimentData,
      impersonation :: Maybe ImpersonationProfile
}
--refactor: this type is being used in a very bonkers way, pls fix his shit
data ExperimentData = Impersonation (Maybe ImpersonationProfile)

data ExperimentPayload = ImpersonationPayload {
      id :: Int,
      sender :: Boolean
}

derive instance genericChatExperimentSection :: Generic ChatExperimentSection _
derive instance genericExperimentCode :: Generic ExperimentData _
derive instance genericExperimentPayload :: Generic ExperimentPayload _

derive instance eqChatExperimentSection :: Eq ChatExperimentSection
derive instance eqExperimentCode :: Eq ExperimentData

instance encodeJsonExperimentPayload :: EncodeJson ExperimentPayload where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonExperimentCode :: EncodeJson ExperimentData where
      encodeJson = DAEGR.genericEncodeJson
instance encodeJsonChatExperimentSection :: EncodeJson ChatExperimentSection where
      encodeJson = DAEGR.genericEncodeJson

instance decodeJsonExperimentCode :: DecodeJson ExperimentData  where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonExperimentPayload :: DecodeJson ExperimentPayload  where
      decodeJson = DADGR.genericDecodeJson
instance decodeJsonChatExperimentSection :: DecodeJson ChatExperimentSection  where
      decodeJson = DADGR.genericDecodeJson

derive instance ordExperimentData  :: Ord ExperimentData

instance boundedExperimentCode :: Bounded ExperimentData where
      bottom = Impersonation Nothing
      top = Impersonation Nothing

instance boundedEnumExperimentCode :: BoundedEnum ExperimentData where
      cardinality = Cardinality 1

      fromEnum = case _ of
          Impersonation _ -> 0

      toEnum = case _ of
          0 -> Just $ Impersonation Nothing
          _ -> Nothing

instance enumExperimentCode :: Enum ExperimentData where
      succ = case _ of
            Impersonation _ -> Nothing

      pred = case _ of
            Impersonation _JsonBoolean -> Nothing

instance showExperimentPayload :: Show ExperimentPayload where
      show = DGRS.genericShow

--placeholder
instance experimentDataFromValue :: FromValue ExperimentData where
      fromValue = Right <<< const (Impersonation Nothing)