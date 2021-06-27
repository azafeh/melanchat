module Shared.User where

import Data.Maybe(Maybe(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Show.Generic as DGRS
import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR

type BasicUser fields = (
      id :: Int,
      name :: String,
      headline :: String,
      description :: String,
      avatar :: Maybe String,
      tags :: Array String,
      karma :: Int,
      karmaPosition :: Int |
      fields
)

type IU = (BasicUser (
      gender :: Maybe String,
      country :: Maybe String,
      languages :: Array String,
      age :: Maybe Int
))
