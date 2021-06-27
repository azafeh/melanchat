module Client.IM.Flame where

import Prelude
import Shared.Types
import Shared.IM.Types
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Flame ((:>))
import Flame as F
import Flame.Application.Effectful as FAE


-- | This action has no further effects
type NoMessages = Tuple IMModel (Array (Aff (Maybe IMMessage)))

-- | This action has further effects
type MoreMessages = Tuple IMModel (Array (Aff (Maybe IMMessage)))

-- | This action has a single further effect
type NextMessage = Tuple IMModel (Array (Aff (Maybe IMMessage)))

-- | Helper to clean up updating a model when no new messages will be raised
diff fields = F.noMessages <<< FAE.diff' fields

-- | Helper to clean up updating a model when new messages will be raised
diffNext fields affs model = FAE.diff' fields model :> affs

-- | Same as pure <<< Just
next = pure <<< Just

-- | Helper to raise a new message without performing an effect
justNext :: IMModel -> IMMessage -> NextMessage
justNext model message = model :> [ pure <<< Just $ message ]

-- Helper to perform a single effect that does not raise a new message
nothingNext :: IMModel -> Aff Unit -> Tuple IMModel (Array (Aff (Maybe IMMessage)))
nothingNext model aff = model :> [
        do
                aff
                pure Nothing
]
