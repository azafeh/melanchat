module Test.Shared.DateTime where

import Prelude

import Data.DateTime as DD
import Data.DateTime as SDT
import Data.Foldable as DA
import Data.Maybe (Maybe(..))
import Data.String.Regex as SR
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe as SRU
import Data.Time.Duration (Days(..))
import Effect.Class (liftEffect)
import Effect.Now as EN
import Shared.DateTime as SD
import Shared.Unsafe as SU
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests :: TestSuite
tests = do
      TU.suite "calculating age" do
            TU.test "ageFrom' handles leap years" do
                  TUA.equal (Just 40) <<< SD.ageFrom' (makeDate 2041 1 2) <<< Just $ makeDate 2000 1 3
                  TUA.equal (Just 40) <<< SD.ageFrom' (makeDate 2041 2 28) <<< Just $ makeDate 2000 2 29
                  TUA.equal (Just 39) <<< SD.ageFrom' (makeDate 2040 2 28) <<< Just $ makeDate 2000 2 29

      TU.suite "displaying message date time" do
            TU.test "ago shows yesterday" do
                  now <- liftEffect EN.nowDateTime
                  TUA.equal "Yesterday" <<< SD.ago <<< SU.fromJust $ SDT.adjust (Days (-1.0)) now

            TU.test "ago shows day of the week" do
                  now <- liftEffect EN.nowDateTime
                  TUA.assert "in day of the week" $ DA.elem (SD.ago <<< SU.fromJust $ SDT.adjust (Days (-2.0)) now) ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

            TU.test "agoWithTime includes time" do
                  now <- liftEffect EN.nowDateTime
                  TUA.assert "matches Yesterday hh:mm" <<< SR.test (SRU.unsafeRegex "Yesterday (\\d{2}):(\\d{2})" noFlags) <<< SD.agoWithTime <<< SU.fromJust $ SDT.adjust (Days (-1.0)) now

      where makeDate y m d = DD.canonicalDate (SU.toEnum y) (SU.toEnum m) (SU.toEnum d)