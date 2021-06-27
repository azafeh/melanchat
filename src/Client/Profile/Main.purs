module Client.Profile.Main where

import Prelude
import Shared.Types

import Client.Common.DOM (setChatExperiment)
import Client.Common.File as CCF
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Shared.IM.Types
import Flame.Application.Effectful as FAE
import Flame.Subscription as FS
import Shared.Options.MountPoint (imID, profileID)
import Shared.Profile.View as SPV
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = do
      FAE.resumeMount (QuerySelector "#profile-edition-form") profileID {
            view: SPV.view,
            subscribe: [FS.onCustomEvent setChatExperiment SetProfileChatExperiment],
            init: Nothing,
            update: CPU.update
      }
      --a pain, but a chat experiment might be going on before loading the modal
      FS.send imID AskChatExperiment
      --avatar changes
      input <- CPU.getFileInput
      CCF.setUpFileChange SetAvatar input profileID
