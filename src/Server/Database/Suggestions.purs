module Server.Database.Suggestions where

import Droplet.Language
import Type.Proxy (Proxy(..))
import Data.DateTime(DateTime)

type Suggestions = (
      id :: Auto Int,
      suggested :: Int,
      score :: Int
)

suggestions :: Table "suggestions" Suggestions
suggestions = Table

_suggested :: Proxy "suggested"
_suggested = Proxy

_score :: Proxy "score"
_score = Proxy
