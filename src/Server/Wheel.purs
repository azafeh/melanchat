-- | Karma score calculation
module Server.Wheel where

import Shared.Types
import Prelude
import Shared.IM.Types
import Data.Int as DI
import Data.Tuple (Tuple(..))

-- | The karma from a chat is calculate from each "turn" in a conversation:
-- | a turn is all messages from sender (the person who started the chat), repicient replies until the next sender message
-- | For each turn, the karma earned is equal to
-- |    gross + gross * sender_bonus
-- | where
-- |       gross = constant * interest_bonus + constant * new_chat_bonus + constant * fast_reply_bonus
-- | The constant can be any number
-- | Interest bonus is the ratio of how many characters one user typed to the other
-- | New chat bonus is how fresh a chat is, more karma should be earned on the first day of a chat than that on the 30000th
-- | Fast reply tries to assert how "instantaneuous" a given conversation is, by making some assumptions about how many characters should have been typed in the time it took for a reply to be sent
-- | The sender gets a bonus on karma earned
karmaFrom :: Turn -> Tuple Int Int
karmaFrom { senderStats, recipientStats, chatAge, replyDelay } =
      let grossSender = karma senderStats
      in Tuple (DI.floor $ grossSender + grossSender * 0.5)  (DI.floor $ karma recipientStats)
  where
    karma { interest, characters } =
        let newChatBonus = (days - min days chatAge) / days
            delayMinutes = max (replyDelay / seconds) 1.0
            fastReplyBonus =
                    (min (characters / minCharacters) charactersRatio * delayMinutes
                        )
                        / (15.0 * delayMinutes)
            interestBonus = min (interest * 0.10) 1.0
        in  constant
                * interestBonus
                + constant
                * newChatBonus
                + constant
                * fastReplyBonus

    constant        = 3.0
    days            = 30.0
    seconds         = 60.0
    minCharacters   = 10.0
    senderBonus     = 2.0
    charactersRatio = 15.0
