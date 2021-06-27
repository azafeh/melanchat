module Shared.Options.MountPoint where

import Flame (AppId(..))
import Shared.Types
import Shared.IM.Types

profileID :: AppId MountPoint ProfileMessage
profileID = AppId Profile

imID :: AppId MountPoint IMMessage
imID = AppId IM