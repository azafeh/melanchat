module Client.IM.Scroll where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Shared.IM.Types
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

scrollLastMessage :: Effect Unit
scrollLastMessage = do
      node <- CCD.unsafeGetElementByID MessageHistory
      CCD.scrollDown node

scrollLastMessage' :: Aff (Maybe IMMessage)
scrollLastMessage' = do
      liftEffect scrollLastMessage
      pure Nothing

scrollIntoView :: HistoryMessage -> Effect Unit
scrollIntoView message = do
      element <- CCD.unsafeQuerySelector <<< ("#m" <> _) <<< show $ message.id
      CCD.scrollIntoView element