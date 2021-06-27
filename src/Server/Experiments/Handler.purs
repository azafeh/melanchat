module Server.Experiments.Handler where

import Prelude
import Server.Types
import Shared.Types

import Run as R
import Server.Experiments.Template as SET
import Shared.Newtype as SN
import Server.Experiments.Database as SED

experiments :: { guards :: { loggedUserID :: Int } } -> ServerEffect String
experiments _ = do
      experiments <- SED.fecthExperiments
      R.liftEffect $ SET.template experiments
