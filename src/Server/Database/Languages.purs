module Server.Database.Languages where

import Type.Proxy (Proxy(..))
import Droplet.Language

type Languages = (
      id :: Auto Int,
      name :: String
)

languages :: Table "languages" Languages
languages = Table
