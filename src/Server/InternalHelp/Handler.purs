module Server.InternalHelp.Handler where

import Shared.Types
import Server.Types
import Server.InternalHelp.Template as SIHT
import Run as R

internalHelp :: { guards :: { loggedUserID :: Int } } -> ServerEffect String
internalHelp _ = R.liftEffect SIHT.template
