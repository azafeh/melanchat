module Server.Profile.Template where

import Prelude
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Shared.Profile.View as SPV

template :: { user :: ProfileUser, countries :: Array {id :: Int, name :: String}, languages :: Array {id :: Int, name :: String} } -> Effect String
template {user: user, countries, languages } = do
      F.preMount (QuerySelector "#profile-edition-form") {
            view: SPV.view,
            init: {
                  nameInputed: Nothing,
                  headlineInputed: Nothing,
                  ageInputed: Nothing,
                  genderInputed: Nothing,
                  countryInputed: Nothing,
                  languagesInputed: Nothing,
                  languagesInputedList: Nothing,
                  tagsInputed: Nothing,
                  tagsInputedList: Nothing,
                  descriptionInputed: Nothing,
                  generating: Nothing,
                  experimenting: Nothing,
                  hideSuccessMessage: false,
                  user,
                  countries,
                  languages
            }
      }

