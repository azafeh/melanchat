module Server.IM.Handler where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Payload.ContentType (html)
import Payload.Headers as PH
import Payload.ResponseTypes (Response)
import Payload.Server.Response as PSR
import Run as R
import Run.Except as RE
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.IM.Template as SIT

im :: { guards :: { loggedUserID :: Int } } -> ServerEffect (Response String)
im { guards: { loggedUserID } } = do
      maybeUser <- SID.presentUser loggedUserID
      case maybeUser of
            --happens if the user has an invalid cookie/was suspended
            Nothing -> RE.throw ExpiredSession
            Just user -> do
                  suggestions <- SIA.suggest loggedUserID 0 Nothing
                  contacts <- SIA.listContacts loggedUserID 0
                  contents <- R.liftEffect $ SIT.template { contacts, suggestions, user: DN.unwrap user }
                  pure <<< PSR.setHeaders (PH.fromFoldable [Tuple "content-type" html, Tuple "cache-control" "no-store, max-age=0"]) $ PSR.ok contents

contacts :: { guards :: { loggedUserID :: Int }, query :: { skip :: Int } } -> ServerEffect (Array Contact)
contacts { guards: { loggedUserID }, query: { skip } } = SIA.listContacts loggedUserID skip

--refactor: maybe contact instead of array contact
singleContact :: { guards :: { loggedUserID :: Int }, query :: { id :: Int } } -> ServerEffect (Array Contact)
singleContact { guards: { loggedUserID }, query: { id } } = DA.singleton <$> SIA.listSingleContact loggedUserID id

history :: { guards :: { loggedUserID :: Int }, query :: { skip :: Int, with :: Int } } -> ServerEffect (Array HistoryMessage)
history { guards: { loggedUserID }, query: { with, skip } } = SIA.resumeChatHistory loggedUserID with skip

suggestions :: { guards :: { loggedUserID :: Int }, query :: { skip :: Int, avoid :: Maybe ArrayPrimaryKey } } -> ServerEffect (Array Suggestion)
suggestions { guards: { loggedUserID }, query: { skip, avoid } } = SIA.suggest loggedUserID skip avoid

block :: { guards :: { loggedUserID :: Int }, body :: { id :: Int } } -> ServerEffect Ok
block { guards: { loggedUserID }, body: { id } } = SIA.blockUser loggedUserID id

missedEvents :: { guards :: { loggedUserID :: Int }, query :: { lastSenderID :: Maybe Int, lastRecipientID :: Maybe Int } } -> ServerEffect MissedEvents
missedEvents { guards: { loggedUserID }, query: { lastSenderID, lastRecipientID } } = SIA.listMissedEvents loggedUserID lastSenderID lastRecipientID

report :: { guards :: { loggedUserID :: Int }, body :: Report } -> ServerEffect Ok
report { guards: { loggedUserID }, body } = SIA.reportUser loggedUserID body