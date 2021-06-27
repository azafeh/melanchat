module Client.IM.Notification where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.IM.Flame (NextMessage)
import Client.IM.Flame as CIF
import Data.Array as DA
import Data.Foldable as DF
import Data.HashMap as HS
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Shared.IM.Types
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Flame.Subscription as FS
import Shared.Experiments.Impersonation (impersonations)
import Shared.IM.Unread as SIU
import Shared.Options.MountPoint (imID)
import Shared.Path as SP
import Shared.Unsafe as SU
import Web.HTML.HTMLLinkElement as WHL

type Notification = {
      body :: String,
      icon :: String,
      -- uses a custom event to let Main know of this
      handler :: Effect Unit
}

foreign import createNotification_ :: EffectFn1 Notification Unit

createNotification :: Notification -> Effect Unit
createNotification = EU.runEffectFn1 createNotification_

notifyUnreadChats :: IMModel -> Array (Tuple Int (Maybe Int)) -> NextMessage
notifyUnreadChats model userIDs = CIF.nothingNext model <<< liftEffect $ notify model userIDs

notify :: IMModel -> Array (Tuple Int (Maybe Int)) -> Effect Unit
notify model@{ user: { id }, contacts, smallScreen } userIDs = do
      updateTabCount id contacts
      unless smallScreen $ DF.traverse_ createNotification' contactUsers
      where contactUsers = DA.filter byKeys contacts
            createNotification' { impersonating, user } = createNotification {
                  body: "New message from " <> (case impersonating of
                        Nothing -> user
                        Just id -> SU.fromJust $ HS.lookup id impersonations).name,
                  icon: SP.pathery PNG "loading",
                  --move to given chat when clicking on system notification
                  handler: FS.send imID <<< ResumeChat $ Tuple user.id impersonating
            }

            byKeys cnt = DA.any (\(Tuple id impersonating) -> cnt.user.id == id && cnt.impersonating == impersonating) userIDs

notify' :: IMModel -> Array (Tuple Int (Maybe Int)) -> Aff (Maybe IMMessage)
notify' model userIDs = do
      liftEffect $ notify model userIDs
      pure Nothing

updateTabCount :: Int -> Array Contact -> Effect Unit
updateTabCount id contacts = do
      CCD.setTitle $ SIU.title unreadChats
      faviconElement <- CCD.unsafeGetElementByID Favicon
      WHL.setHref (SIU.favicon unreadChats) <<< SU.fromJust $ WHL.fromElement faviconElement
      where unreadChats = SIU.countUnreadChats id contacts