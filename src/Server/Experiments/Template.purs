module Server.Experiments.Template where

import Shared.Types

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame (QuerySelector(..))
import Shared.Experiments.Types
import Flame as F
import Shared.Experiments.View as SEV

template :: Array ChatExperiment -> Effect String
template experiments = F.preMount (QuerySelector ".chat-experiments") {
      view: SEV.view,
      init: {
            experiments,
            section: HideSections,
            impersonation: Nothing,
            current : Nothing
      }
}
