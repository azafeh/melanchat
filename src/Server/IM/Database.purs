module Server.IM.Database where

import Prelude
import Server.Types
import Shared.Types

import Server.Database.Messages
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String.Common as DS
import Droplet.Driver(Pool)
import Server.Database.Reports
import Server.Database.Blocks
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Shared.IM.Types
import Shared.Unsafe as SU
import Droplet.Language
import Server.Database.Fields
import Server.Database as SD
import Shared.Options.Page (contactsPerPage, messagesPerPage, initialMessagesPerPage, suggestionsPerPage)

userPresentationFields :: String
userPresentationFields = """ u.id,
avatar,
gender,
date_part('year', age(now() at time zone 'utc', birthday)) as age,
name,
headline,
description,
(select name from countries where id = country) country,
(select string_agg(l.name, ','  order by name) from languages l join languages_users lu on l.id = lu.language and lu.speaker = u.id ) languages,
(select string_agg(name, '\n' order by l.id) from tags l join tags_users tu on l.id = tu.tag and tu.creator = u.id ) tags,
k.current_karma karma,
k.position """

usersTable :: String
usersTable = " users u join karma_leaderboard k on u.id = k.ranker "

messagePresentationFields :: String
messagePresentationFields = " id, sender, recipient, date, content, status "

presentUserQuery = "select" <> userPresentationFields <> "from" <> usersTable <> "where u.id = @id and active"

presentUser :: Int -> ServerEffect (Maybe IMUser)
presentUser loggedUserID = SD.unsafeSingle presentUserQuery {id: loggedUserID}

--fit online status here
suggest :: Int -> Int -> Maybe ArrayPrimaryKey -> ServerEffect (Array IMUser)
suggest loggedUserID skip = case _ of
      Just (ArrayPrimaryKey []) ->
            SD.unsafeQuery (select <> rest) {id: loggedUserID, page: suggestionsPerPage, skip}
      Just (ArrayPrimaryKey keys) ->
            SD.unsafeQuery (select <> "and not (u.id = any(@keys))" <> rest) {id: loggedUserID, page: suggestionsPerPage, skip, keys}
      _ ->
            SD.unsafeQuery (select <> "and not exists(select 1 from histories where sender in (@id, u.id) and recipient in (@id, u.id))" <> rest) {id: loggedUserID, page: suggestionsPerPage, skip}
      where select = "select * from (select" <> userPresentationFields <> "from"  <> usersTable <> "join suggestions s on u.id = suggested where u.id <> @id "
            rest = " and u.active and not exists (select 1 from blocks where blocker in (@id, u.id) and blocked in (@id, u.id)) order by s.id limit @page offset @skip) t order by random()"

--should not return contact
presentContacts :: Int -> Int -> ServerEffect (Array Contact)
presentContacts loggedUserID skip = SD.unsafeQuery (("select distinct h.date, sender, date_part('day', age(now() at time zone 'utc', first_message_date)), " <> userPresentationFields <>
                                      "from" <> usersTable <> """join histories h on (u.id = h.sender and h.recipient = $1 or u.id = h.recipient and h.sender = $1)
                                         where not exists (select 1 from blocks where blocker = h.recipient and blocked = h.sender or blocker = h.sender and blocked = h.recipient)
                                          order by date desc limit $2 offset $3""")) (loggedUserID /\ contactsPerPage /\ skip)

--needs to handle impersonations
presentSingleContact :: Int -> Int -> ServerEffect Contact
presentSingleContact loggedUserID otherID = SU.fromJust <$> SD.unsafeSingle (("select coalesce(h.date, now() at time zone 'utc'), coalesce(sender, @otherID), coalesce(first_message_date, now() at time zone 'utc'), " <> userPresentationFields <>
                                      "from" <> usersTable <> "left join histories h on (u.id = h.recipient and h.sender = @id or u.id = h.sender and h.recipient = @id) where u.id = @otherID")) {id: loggedUserID, otherID}

presentSelectedContacts :: Int -> Array Int -> ServerEffect (Array Contact)
presentSelectedContacts loggedUserID ids
      | DA.null ids = pure []
      | otherwise = SD.unsafeQuery ("select distinct h.date, sender, first_message_date," <> userPresentationFields <> "from" <> usersTable <> "join histories h on (u.id = h.sender and h.recipient = $1 or u.id = h.recipient and h.sender = $1) where u.id = any($2)") (loggedUserID /\ ids)

--there must be a better way to do this
chatHistoryFor :: Int -> Array Int -> ServerEffect (Array HistoryMessage)
chatHistoryFor loggedUserID otherIDs
      | DA.null otherIDs = pure []
      | otherwise = SD.unsafeQuery (query) (loggedUserID /\ initialMessagesPerPage /\ Delivered)
      where query = "select * from (" <> DS.joinWith " union all " (select <$> otherIDs) <> ") r order by date, sender, recipient"
            select n =
                  let parameter = show n
                  in "((select" <> messagePresentationFields <> "from messages where sender = $1 and recipient = " <> parameter <> " or sender = " <> parameter <> " and recipient = $1 order by date desc limit $2) union (select" <> messagePresentationFields <> "from messages where recipient = $1 and sender = " <> parameter <> " and status < $3 order by date desc))"

chatHistorySince :: Int -> Int -> ServerEffect (Array HistoryMessage)
chatHistorySince loggedUserID lastID = SD.query $ select (_id /\ _sender /\ _recipient /\ _date /\ _content /\ _status) # from messages # wher (_recipient .=. loggedUserID .&&. _id .>. lastID .&&. _status .<. Delivered) # orderBy (_date /\ _sender)

chatHistoryBetween :: Int -> Int -> Int -> ServerEffect (Array HistoryMessage)
chatHistoryBetween loggedUserID otherID skip = SD.unsafeQuery (("select * from (select" <> messagePresentationFields <> "from messages where sender = $1 and recipient = $2 or sender = $2 and recipient = $1 order by date desc limit $3 offset $4) s order by date")) (loggedUserID /\ otherID /\ messagesPerPage /\ skip)

messsageIDsFor :: Int -> Int -> ServerEffect (Array MessageIDTemporary)
messsageIDsFor loggedUserID messageID = SD.query $ select (_id /\ _temporary_id) # from messages # wher (_sender .=. loggedUserID .&&. id .>. messageID)

insertMessage :: forall r. Int -> Int -> Int -> String -> BaseEffect { pool :: Pool | r } Int
insertMessage loggedUserID recipient temporaryID content = SD.withTransaction $ \connection -> do
      SD.unsafeExecuteWith connection "select insert_history(@sender, @recipient)" { sender : loggedUserID, recipient }
      SD.executeWith connection $ insert # into messages (_sender /\ _recipient /\ _temporary_id /\ _content) # values (loggedUserID /\ recipient /\ temporaryID /\ content)

insertKarma :: forall r. Int -> Int -> Tuple Int Int -> BaseEffect { pool :: Pool | r } Unit
insertKarma loggedUserID otherID (Tuple senderKarma recipientKarma) =
      void $ SD.unsafeExecute "insert into karma_histories(amount, target) values (@senderKarma, @senderID), (@recipientKarma, @recipientID)" ({senderKarma, senderID: loggedUserID, recipientKarma, recipientID: otherID})

--when using an array parameter, any must be used instead of in
changeStatus :: forall r. Int -> MessageStatus -> Array Int -> BaseEffect { pool :: Pool | r } Unit
changeStatus loggedUserID status ids = SD.unsafeExecute "update messages set status = @status where recipient = @recipient and id = any(@ids)" {status, recipient: loggedUserID, ids}

insertBlock :: Int -> Int -> ServerEffect Unit
insertBlock loggedUserID blocked = void $ SD.execute blockQuery (loggedUserID /\ blocked)

blockQuery blocker blocked = insert # into blocks (_blocker /\ _blocked) # values (blocker /\ blocked)

insertReport :: Int -> Report -> ServerEffect Unit
insertReport loggedUserID { userID, comment, reason } = SD.withTransaction $ \connection -> do
      SD.executeWith connection $ blockQuery loggedUserID userID
      SD.executeWith connection $ insert # into reports (_reporter /\ _reported /\ _reason /\ _comment) # values (loggedUserID /\ userID /\ reason /\ comment)
