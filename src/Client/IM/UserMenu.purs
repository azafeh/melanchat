module Client.IM.UserMenu where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Logout as CCLO
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages, NoMessages)
import Client.IM.Flame as CIF
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.Newtype as SN
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.Event.Event (Event)
import Web.Event.Event as WEE

logout :: Boolean -> IMModel -> MoreMessages
logout confirmed model = CIF.nothingNext model <<< liftEffect $ when confirmed CCLO.logout

confirmLogout :: IMModel -> NoMessages
confirmLogout = (_ :> [Just <<< Logout <$> liftEffect (CCD.confirm "Really log out?")])

--PERFORMANCE: load bundles only once
toggleProfileSettings :: ProfileSettingsToggle -> IMModel -> MoreMessages
toggleProfileSettings psToggle model =
      case psToggle of
            ShowProfile -> showTab request.profile.get ShowProfile "profile.bundle.js" "#profile-edition-root"
            ShowSettings -> showTab request.settings.get ShowSettings "settings.bundle.js" "#settings-edition-root"
            Hidden -> CIF.justNext (model { profileSettingsToggle = Hidden }) $ SetModalContents Nothing "#profile-edition-root" "Loading..."
      where showTab f toggle file root =
                  model { profileSettingsToggle = toggle } :> [
                        Just <<< SetModalContents (Just file) root <$> CCN.response (f {})
                  ]

loadModal :: String -> String -> Maybe String -> Aff Unit
loadModal root html file = liftEffect do
      element <- CCD.querySelector root
      CCD.setInnerHTML element html
      --scripts don't load when inserted via innerHTML
      case file of
            Just name -> CCD.loadScript name
            Nothing -> pure unit

showUserContextMenu :: Event -> IMModel -> MoreMessages
showUserContextMenu event model@{ userContextMenuVisible }
      | userContextMenuVisible =
            F.noMessages $ model { userContextMenuVisible = false }
      | otherwise =
            model :> [
                  liftEffect <<< map (Just <<< SetUserContentMenuVisible <<< (_ == "user-context-menu")) $ WDE.id <<< SU.fromJust $ do
                  target <- WEE.target event
                  WDE.fromEventTarget target
            ]
