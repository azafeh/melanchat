module Client.Experiments.Update where

import Prelude
import Shared.Types

import Client.Common.DOM (setChatExperiment)
import Client.Common.Location as CCL
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Flame (ListUpdate, (:>))
import Shared.Experiments.Types
import Flame as F
import Flame.Subscription.Unsafe.CustomEvent as FSUC

update :: ListUpdate ChatExperimentModel ChatExperimentMessage
update model =
      case _ of
            QuitExperiment -> model :> [ do
                  liftEffect CCL.reload
                  pure Nothing
            ]
            JoinExperiment code -> model {
                  section = HideSections,
                  current = Just code
            } :> dispatchEvent (Just code)
            ToggleSection section -> F.noMessages $ model { section = section }
            ConfirmImpersonation profile -> F.noMessages model { impersonation = profile }
      where dispatchEvent payload = [ liftEffect do
                  FSUC.broadcast setChatExperiment payload
                  pure Nothing
            ]