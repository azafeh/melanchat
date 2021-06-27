module Client.IM.Suggestion where

import Prelude
import Shared.Types

import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (NextMessage, NoMessages, MoreMessages)
import Client.IM.WebSocket as CIW
import Data.Array ((:))
import Shared.IM.Types
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Debug (spy)
import Effect.Class (liftEffect)
import Shared.Experiments.Types
import Flame ((:>))
import Flame as F
import Shared.Options.Page (suggestionsPerPage)
import Web.Socket.WebSocket (WebSocket)

nextSuggestion :: IMModel -> MoreMessages
nextSuggestion model@{ suggestions, suggesting } =
      let next = DM.maybe 0 (_ + 1) suggesting
      in    if next == DA.length suggestions then
                  fetchMoreSuggestions model
             else
                  F.noMessages $ model {
                        freeToFetchSuggestions = true,
                        suggesting = Just next,
                        chatting = Nothing
                  }

previousSuggestion :: IMModel -> MoreMessages
previousSuggestion model@{ suggesting } =
      let previous = DM.maybe 0 (_ - 1) suggesting
      in
            if previous < 0 then
                  fetchMoreSuggestions model
             else
                  F.noMessages $ model  {
                        freeToFetchSuggestions = true,
                        suggesting = Just previous,
                        chatting = Nothing
                  }

fetchMoreSuggestions :: IMModel -> NextMessage
fetchMoreSuggestions model@{ contacts, suggestionsPage, experimenting } = model {
      freeToFetchSuggestions = false,
      failedRequests = []
} :> [CCN.retryableResponse NextSuggestion DisplayMoreSuggestions $ request.im.suggestions {
            query: {
                  skip: suggestionsPerPage * suggestionsPage,
                  avoid: case experimenting of
                        Just (Impersonation (Just _)) -> Just <<< ArrayPrimaryKey $ map (_.id <<< _.user) contacts
                        _ -> Nothing
            }
      }]

displayMoreSuggestions :: Array Suggestion -> IMModel -> MoreMessages
displayMoreSuggestions suggestions model@{ suggestionsPage } =
      --if we looped through all the suggestions, retry
      if suggestionsSize == 0 && suggestionsPage > 0 then
            fetchMoreSuggestions $ model {
                  suggestionsPage = 0,
                  suggesting = suggesting
            }
      else
            F.noMessages $ model {
                  suggesting = suggesting,
                  chatting = Nothing,
                  freeToFetchSuggestions = true,
                  suggestions = suggestions,
                  suggestionsPage = if suggestionsSize == 0 then 0 else suggestionsPage + 1
            }
      where suggestionsSize = DA.length suggestions
            suggesting = Just $ if suggestionsSize <= 1 then 0 else 1

blockUser :: WebSocket  -> Int -> IMModel -> NextMessage
blockUser webSocket blocked model@{ blockedUsers } =
      updateAfterBlock blocked model :> [do
            result <- CCN.defaultResponse $ request.im.block { body: { id: blocked } }
            case result of
                  --refactor: either make errorMessage maybe or get rid of it
                  Left _ -> pure <<< Just $ RequestFailed { request: BlockUser blocked, errorMessage : "" }
                  _ -> do
                        liftEffect <<< CIW.sendPayload webSocket $ ToBlock { id: blocked }
                        pure Nothing
      ]

updateAfterBlock :: Int -> IMModel -> IMModel
updateAfterBlock blocked model@{ contacts, suggestions, blockedUsers } =
      model {
            contacts = DA.filter ((blocked /= _) <<< fromContact) contacts,
            suggestions = DA.filter ((blocked /= _) <<< fromUser) suggestions,
            blockedUsers = blocked : blockedUsers,
            chatting = Nothing,
            failedRequests = [],
            toggleModal = HideUserMenuModal,
            toggleContextMenu = HideContextMenu
      }
      where fromContact { user } = fromUser user
            fromUser { id } = id

toggleContactProfile :: IMModel -> NoMessages
toggleContactProfile model@{ fullContactProfileVisible } = F.noMessages $ model {
      fullContactProfileVisible = not fullContactProfileVisible
}

resumeSuggesting :: IMModel -> NoMessages
resumeSuggesting model@{ suggestions, suggesting } = F.noMessages $ model {
      chatting = Nothing,
      suggesting = if DA.length suggestions <= 1 then Just 0 else suggesting
}