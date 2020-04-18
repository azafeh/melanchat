module Client.IM.UserMenu where

import Prelude
import Shared.IM.Types

import Client.Common.DOM as CCD
import Client.Common.Location as CCL
import Client.Common.Network as CCN
import Client.Common.Storage (tokenKey)
import Client.Common.Storage as CCS
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as F
import Flame.Application.Effectful as FAE
import Client.Common.Cookies as CCC
import Shared.Router as SR
import Shared.Types (JSONString(..), Route(..))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.Event.Event (Event)
import Web.Event.Event as WEE

update :: AffUpdate IMModel UserMenuMessage
update environment@{ model, message } =
        case message of
                Logout -> do
                        liftEffect logout
                        FAE.noChanges
                ShowUserContextMenu event -> F.diff' <$> showUserContextMenu model event
                ShowProfile -> showProfile environment

showProfile :: AffUpdate IMModel UserMenuMessage
showProfile { display, model } = do
        display $ F.diff' { profileEditionVisible: true }
        JSONString html <- CCN.get' $ SR.fromRouteAbsolute Profile
        liftEffect do
                element <- CCD.querySelector ".profile-edition"
                CCD.setInnerHTML element html
        FAE.noChanges

showUserContextMenu :: IMModel -> Event -> Aff { userContextMenuVisible :: Boolean }
showUserContextMenu model@(IMModel { userContextMenuVisible }) event = do
        shouldBeVisible <-
                if userContextMenuVisible then
                        pure false
                 else
                        liftEffect <<< map ( _ == "user-context-menu") $ WDE.id <<< SU.unsafeFromJust "usermenu.update" $ do
                                target <- WEE.target event
                                WDE.fromEventTarget target
        pure {
                userContextMenuVisible: shouldBeVisible
        }

logout :: Effect Unit
logout = do
        confirmed <- CCD.confirm "Really log out?"
        when confirmed $ void do
                CCS.removeItem tokenKey
                CCC.removeMelanchatCookie
                CCL.setLocation "/"
