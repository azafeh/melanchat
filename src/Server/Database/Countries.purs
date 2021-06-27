module Server.Database.Countries where

import Type.Proxy (Proxy(..))
import Droplet.Language

type Countries = (
      id :: Auto Int,
      name :: String
)

countries :: Table "countries" Countries
countries = Table
