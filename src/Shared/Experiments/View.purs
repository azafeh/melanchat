module Shared.Experiments.View where

import Prelude
import Shared.Types

import Data.Maybe (Maybe(..))
import Environment (experimentsCSSHash)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Types
import Shared.Experiments.Impersonation as SEI
import Shared.Path as SP

view :: ChatExperimentModel -> Html ChatExperimentMessage
view model@{ experiments, section, current } = case current of
      Just (Impersonation (Just profile)) ->
            --likely to be the same for all experiments
            HE.div (HA.class' "chat-experiments") $ SEI.joined profile
      _ ->
            HE.div (HA.class' "chat-experiments") [
                  HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href <<< SP.pathery CSS $ "experiments." <> experimentsCSSHash],
                  HE.span (HA.class' "duller") "Choose a chat experiment from the list bellow. The experiment will last until you leave it or refresh the page",
                  HE.div (HA.class' "all-experiments")  $ map toDiv experiments
            ]
      where toDiv {name, description, code} = HE.div (HA.class' "experiment") [
                  HE.a (HA.class' "experiment-name") name,
                  HE.span_ description,
                  HE.fragment $ extra model code
            ]

extra :: ChatExperimentModel -> ExperimentData -> Html ChatExperimentMessage
extra model = case _ of
      Impersonation _ -> SEI.view model