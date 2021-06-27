module Server.Database.Reports where

import Type.Proxy (Proxy(..))
import Droplet.Language
import Data.DateTime(DateTime)

type Reports = (
      id :: Auto Int,
      comment :: String,
      reason :: Int, --should be enum
      reporter :: Int,
      date :: Default DateTime,
      reported :: Int
)

reports :: Table "reports" Reports
reports = Table

_comment :: Proxy "comment"
_comment = Proxy

_reason :: Proxy "reason"
_reason = Proxy

_reporter :: Proxy "reporter"
_reporter = Proxy

_reported :: Proxy "reported"
_reported = Proxy
