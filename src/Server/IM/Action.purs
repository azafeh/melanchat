--refactor: here and other modueles only export whats needed
module Server.IM.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.Foldable as DF
import Data.Foldable as FD
import Data.HashMap as DH
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple (Tuple(..))
import Data.UUID as DU

import Effect.Ref (Ref)
import Environment (development)
import Node.Buffer as NB
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NFS
import Run as R
import Server.Email as SE
import Server.File as SF
import Server.IM.Database as SID
import Server.Ok (ok)
import Server.Response as SR
import Server.Wheel as SW
import Shared.Newtype as SN
import Shared.Options.File (allowedMediaTypes, imageBasePath, maxImageSize)
import Shared.Options.File (imageBasePath)
import Shared.Unsafe as SU

foreign import sanitize :: String -> String

suggest :: Int -> Int -> Maybe ArrayPrimaryKey -> ServerEffect (Array Suggestion)
suggest loggedUserID skip = SN.unwrapAll <<< SID.suggest loggedUserID skip

listContacts :: Int -> Int -> ServerEffect (Array Contact)
listContacts loggedUserID skip = do
      contacts <- SN.unwrapAll $ SID.presentContacts loggedUserID skip
      history <- SN.unwrapAll <<< SID.chatHistoryFor loggedUserID $ map (_.id <<< _.user) contacts
      let userHistory = DF.foldl intoHashMap DH.empty history
      pure $ intoContacts userHistory <$> contacts

      where intoHashMap hashMap m@{ sender, recipient } =
                  DH.insertWith (<>) (if sender == loggedUserID then recipient else sender) [m] hashMap

            intoContacts userHistory contact@{ user: { id: loggedUserID } } = contact {
                  history = SU.fromJust $ DH.lookup loggedUserID userHistory
            }

listSingleContact :: Int -> Int -> ServerEffect Contact
listSingleContact loggedUserID userID = do
      contact <- DN.unwrap <$> SID.presentSingleContact loggedUserID userID
      history <- SN.unwrapAll $ SID.chatHistoryBetween loggedUserID userID 0
      pure $ contact {
            history = history
      }

processMessage :: forall r. Int -> Int -> Int -> MessageContent -> BaseEffect { storageDetails :: Ref StorageDetails, pool :: Pool | r } (Tuple Int String)
processMessage loggedUserID userID temporaryID content = do
      sanitized <- processMessageContent content
      id <- SID.insertMessage loggedUserID userID temporaryID sanitized
      pure $ Tuple id sanitized

-- | Sanitizes markdown and handle image uploads
processMessageContent :: forall r. MessageContent -> BaseEffect { storageDetails :: Ref StorageDetails, pool :: Pool | r } String
processMessageContent content = do
      message <- case content of
            Text m -> pure m
            Image caption base64 -> do
                  path <- SF.saveBase64File $ base64
                  pure $ "![" <> caption  <> "](" <> imageBasePath <> "upload/" <> path <> ")"
      pure <<< DS.trim $ sanitize message

processKarma :: forall r. Int -> Int -> Turn -> BaseEffect { pool :: Pool | r } Unit
processKarma loggedUserID userID turn = SID.insertKarma loggedUserID userID $ SW.karmaFrom turn

blockUser :: Int -> Int -> ServerEffect Ok
blockUser loggedUserID userID = do
      SID.insertBlock loggedUserID userID
      pure ok

listMissedEvents :: Int -> Maybe Int -> Maybe Int -> ServerEffect MissedEvents
listMissedEvents loggedUserID lastSenderID lastRecipientID = do
      messageIDs <- SN.unwrapAll $ DM.maybe (pure []) (SID.messsageIDsFor loggedUserID) lastSenderID
      history <- SN.unwrapAll $ DM.maybe (pure []) (SID.chatHistorySince loggedUserID) lastRecipientID
      contacts <- SN.unwrapAll <<< SID.presentSelectedContacts loggedUserID <<< DA.nubEq $ map _.sender history
      let userHistory = DF.foldl intoHashMap DH.empty history
      pure {
            contacts: intoContacts userHistory <$> contacts,
            messageIDs
      }

      where intoHashMap hashMap m@{ sender } = DH.insertWith (<>) sender [m] hashMap

            intoContacts userHistory contact@{ user: { id } } = contact {
                  history = SU.fromJust $ DH.lookup id userHistory
            }

resumeChatHistory :: Int -> Int -> Int -> ServerEffect (Array HistoryMessage)
resumeChatHistory loggedUserID userID skip = SN.unwrapAll $ SID.chatHistoryBetween loggedUserID userID skip

reportUser :: Int -> Report -> ServerEffect Ok
reportUser loggedUserID report@{ reason, userID } = do
      SID.insertReport loggedUserID report
      SE.sendEmail "contact@melan.chat" ("[REPORT] " <> show reason) $ "select * from reports where reported = " <> show userID <> ";"
      pure ok