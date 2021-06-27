module Server.InternalBacker.Handler where

import Shared.Types
import Server.Types
import Server.InternalBacker.Template as SIBT
import Run as R

internalBacker :: { guards :: { loggedUserID :: Int } } -> ServerEffect String
internalBacker _ = R.liftEffect SIBT.template
