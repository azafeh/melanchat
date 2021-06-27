module Server.Database.LanguagesUsers where

import Type.Proxy (Proxy(..))
import Droplet.Language

type LanguagesUsers = (
      id :: Auto Int,
      speaker :: Int,
      language :: Int
)

languages_users :: Table "languages_users" LanguagesUsers
languages_users = Table


_speaker :: Proxy "speaker"
_speaker = Proxy

_language :: Proxy "language"
_language = Proxy