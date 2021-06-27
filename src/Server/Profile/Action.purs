--missing tests
module Server.Profile.Action where

import Prelude
import Server.Types hiding (ThreeKAction(..))
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.String (Pattern(..), Replacement(..))
import Data.String as DS
import Run as R
import Server.ThreeK as SB
import Server.File as SF
import Server.Ok (ok)
import Server.Profile.Database as SPD
import Server.Response as SR
import Shared.DateTime as SDT
import Shared.Options.File (imageBasePath)
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, maxLanguages, maxTags, nameMaxCharacters, tagMaxCharacters)

missingRequiredFieldsMessage :: String
missingRequiredFieldsMessage = "Name, headline and description are mandatory"

tooManyTagsMessage :: String
tooManyTagsMessage = "Number of tags larger than " <> show maxTags <> " limit"

tooManyLanguagesMessage :: String
tooManyLanguagesMessage = "Number of languages larger than " <> show maxLanguages <> " limit"

tooYoungMessage :: String
tooYoungMessage = "You must be over 13 years old in order to use MelanChat"

fieldTooBigMessage :: String
fieldTooBigMessage = "Field exceeded max value"

generate :: Generate -> ServerEffect String
generate =
      case _ of
            Name -> SB.generateName
            Headline -> SB.generateHeadline
            Description -> SB.generateDescription

saveProfile :: Int -> ProfileUser -> ServerEffect Ok
saveProfile loggedUserID profileUser@{ name, age, headline, description, avatar, languages, tags } = do
      when (isNull name || isNull headline || isNull description) $ SR.throwBadRequest missingRequiredFieldsMessage
      when anyFieldIsTooBig $ SR.throwBadRequest fieldTooBigMessage
      when (DA.length tags > maxTags) $ SR.throwBadRequest tooManyTagsMessage
      when (DA.length languages > maxLanguages) $ SR.throwBadRequest tooManyLanguagesMessage
      thirteen <- Just <$> R.liftEffect SDT.latestEligibleBirthday
      when (map DN.unwrap age > thirteen) $ SR.throwBadRequest tooYoungMessage

      updatedAvatar <- case avatar of
            Nothing -> pure Nothing
            Just path ->
                  let fileName = DS.replace (Pattern $ imageBasePath <> "upload/") (Replacement "") path in
                  --likely a base64 image
                  if fileName == path then
                        Just <$> SF.saveBase64File path
                   else
                        pure $ Just fileName
      SPD.saveProfile {
            user: profileUser { id = loggedUserID },
            avatar: updatedAvatar,
            languages,
            tags
      }
      pure ok
      where isNull = DS.null <<< DS.trim
            anyFieldIsTooBig = DS.length name > nameMaxCharacters || DS.length headline > headlineMaxCharacters || DS.length description > descriptionMaxCharacters || DA.any ((_ > tagMaxCharacters) <<< DS.length) tags