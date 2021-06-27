module Client.IM.Chat where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.IM.Flame (MoreMessages, NoMessages, NextMessage)
import Client.IM.Flame as CIF
import Client.IM.Scroll as CIS
import Client.IM.WebSocket as CIW
import Data.Array ((!!))
import Data.Array as DA
import Data.DateTime as DT
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.Nullable (null)
import Data.String as DS

import Data.String.CodeUnits as DSC
import Type.Proxy(Proxy(..))
import Data.Symbol as TDS
import Data.Time.Duration (Seconds)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Shared.IM.Types
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU
import Flame ((:>))
import Flame as F
import Node.URL as NU
import Shared.IM.Contact as SIC
import Shared.Experiments.Types
import Shared.Markdown as SM
import Shared.Options.File (maxImageSize)
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.DOM.Element as WDE
import Web.Event.Event (Event)
import Web.Event.Event as WEE
import Web.File.FileReader (FileReader)
import Web.HTML.Event.DataTransfer as WHEDT
import Web.HTML.Event.DragEvent as WHED
import Web.HTML.HTMLElement as WHHEL
import Web.HTML.HTMLTextAreaElement as WHHTA
import Web.Socket.WebSocket (WebSocket)

foreign import resizeTextarea_ :: EffectFn1 Element Unit

-- this event is filterd to run only on Enter keydown
enterBeforeSendMessage :: Event -> IMModel -> NoMessages
enterBeforeSendMessage event model@{ messageEnter } =
      model :> if messageEnter then [prevent, getMessage model] else []
      where prevent = liftEffect do
                  WEE.preventDefault event
                  pure Nothing

getMessage :: IMModel -> Aff (Maybe IMMessage)
getMessage model@{ selectedImage, imageCaption } = do
      input <- liftEffect $ chatInput model
      value <- liftEffect $ CCD.value input
      pure <<< Just <<< BeforeSendMessage $ DM.maybe (Text value) toImage selectedImage
      where toImage base64File = Image (DM.fromMaybe "" imageCaption) base64File

--send message/image button
forceBeforeSendMessage :: IMModel -> MoreMessages
forceBeforeSendMessage model =
      model :> [
            getMessage model,
            resizeInputEffect model
      ]

resizeInputEffect :: IMModel -> Aff (Maybe IMMessage)
resizeInputEffect model = liftEffect $ do
      input <- chatInput model
      resizeTextarea input
      pure Nothing

beforeSendMessage :: MessageContent -> IMModel -> MoreMessages
beforeSendMessage content model@{
      chatting,
      user: { id },
      selectedImage,
      contacts,
      suggesting,
      suggestions,
      experimenting
} = case content of
      t@(Text message)
            | isEmpty message -> F.noMessages model
            | otherwise -> snocContact :> [ nextSendMessage t ]
      image -> snocContact :> [ nextSendMessage image ]

      where isEmpty = DS.null <<< DS.trim

            snocContact = case chatting, suggesting of
                  Nothing, (Just index) ->
                        model {
                              chatting = Just 0,
                              contacts = DA.cons (SIC.defaultContact id (suggestions !@ index)) contacts,
                              suggestions = SU.fromJust $ DA.deleteAt index suggestions
                        }
                  _, _ -> model

            nextSendMessage input = do
                  date <- liftEffect $ map DateTimeWrapper EN.nowDateTime
                  CIF.next $ SendMessage input date

sendMessage :: WebSocket -> MessageContent -> DateTimeWrapper -> IMModel -> NoMessages
sendMessage webSocket content date model@{
      user: { id: senderID },
      chatting,
      temporaryID,
      contacts,
      imageCaption,
      experimenting
} = CIF.nothingNext updatedModel $ liftEffect do
      CIS.scrollLastMessage
      input <- chatInput model
      WHHEL.focus <<< SU.fromJust $ WHHEL.fromElement input
      CCD.setValue input ""
      CIW.sendPayload webSocket $ OutgoingMessage {
            id: newTemporaryID,
            userID: recipientID,
            content,
            experimenting: case experimenting, recipient.impersonating of
                  Just (Impersonation (Just { id })), _ -> Just $ ImpersonationPayload { id: id, sender: true }
                  _, Just id -> Just $ ImpersonationPayload { id: id, sender: false }
                  _, _ -> Nothing,
            turn
      }
      where index = SU.fromJust chatting
            recipient@{ user: { id: recipientID }, history } = contacts !@ index
            newTemporaryID = temporaryID + 1

            updatedContact = recipient {
                  history = DA.snoc history $  {
                        id: newTemporaryID,
                        status: Sent,
                        sender: senderID,
                        recipient: recipientID,
                        date,
                        content: case content of
                              Text message -> message
                              Image caption base64File -> asMarkdownImage imageCaption base64File
                  }
            }
            updatedModel = model {
                  temporaryID = newTemporaryID,
                  imageCaption = Nothing,
                  selectedImage = Nothing,
                  contacts = SU.fromJust $ DA.updateAt index updatedContact contacts
            }
            turn = makeTurn updatedContact senderID

            asMarkdownImage imageCaption base64 = "![" <> DM.fromMaybe "" imageCaption  <> "](" <> base64 <> ")"

makeTurn :: Contact -> Int -> Maybe Turn
makeTurn { chatStarter, chatAge, history } sender =
      if chatStarter == sender && isNewTurn history sender then
            let   senderEntry = SU.fromJust $ DA.last history
                  recipientEntry = SU.fromJust $ history !! (DA.length history - 2)
                  Tuple senderMessages recipientMessages = SU.fromJust do
                        let   groups = DA.groupBy sameSender history
                              size = DA.length groups
                              beforeLastIndex = (max size 3) - 1 - 2
                        senderMessages <- groups !! beforeLastIndex
                        recipientMessages <- DA.last groups
                        pure $ Tuple senderMessages recipientMessages
                  senderCharacters = DI.toNumber $ DA.foldl countCharacters 0 senderMessages
                  recipientCharacters = DI.toNumber $ DA.foldl countCharacters 0 recipientMessages
            in
                  Just {
                        senderStats: {
                              characters: senderCharacters,
                              interest: senderCharacters / recipientCharacters
                        },
                        recipientStats: {
                              characters: recipientCharacters,
                              interest: recipientCharacters / senderCharacters
                        },
                        replyDelay: DN.unwrap (DT.diff (getDate senderEntry) $ getDate recipientEntry :: Seconds),
                        chatAge
                  }
       else
            Nothing
      where isNewTurn history userID = DM.fromMaybe false do
                  last <- DA.last history
                  beforeLast <- history !! (DA.length history - 2)
                  let sender = last.sender
                      recipient = beforeLast.recipient
                  pure $ sender == userID && recipient == userID

            sameSender entry anotherEntry = entry.sender ==  anotherEntry.sender
            countCharacters total ( { content }) = total + DSC.length content
            getDate = DN.unwrap <<< _.date

applyMarkup :: Markup -> IMModel -> MoreMessages
applyMarkup markup model =
      model :> [
            liftEffect (Just <$> apply markup),
            resizeInputEffect model
      ]
      where apply markup = do
                  input <- chatInput model
                  value <- CCD.value input
                  let   textarea = SU.fromJust $ WHHTA.fromElement input
                        Tuple before after = case markup of
                              Bold -> Tuple "**" "**"
                              Italic -> Tuple "*" "*"
                              Strike -> Tuple "~" "~"
                              Heading -> Tuple "\n## " ""
                              OrderedList -> Tuple "\n1. " ""
                              UnorderedList -> Tuple "\n- " ""
                  start <- WHHTA.selectionStart textarea
                  end <- WHHTA.selectionEnd textarea
                  let   beforeSize = DS.length before
                        beforeSelection = DS.take start value
                        selected = DS.take (end - start) $ DS.drop start value
                        afterSelection = DS.drop end value
                        newValue = beforeSelection <> before <> selected <> after <> afterSelection
                  pure $ SetMessageContent (Just $ end + beforeSize) newValue

chatInput :: IMModel -> Effect Element
chatInput model@{ chatting }
      | DM.isNothing chatting = CCD.unsafeGetElementByID ChatInputSuggestion -- suggestion card input cannot be cached
      | otherwise = CCD.unsafeGetElementByID ChatInput

setMessage :: Maybe Int -> String -> IMModel -> NextMessage
setMessage cursor markdown model =
      CIF.nothingNext model <<< liftEffect $
            case cursor of
                  Just position -> do
                        input <- chatInput model
                        let textarea = SU.fromJust $ WHHTA.fromElement input
                        WHHEL.focus $ WHHTA.toHTMLElement textarea
                        WHHTA.setValue markdown textarea
                        WHHTA.setSelectionEnd position textarea
                  Nothing -> pure unit

catchFile :: FileReader -> Event -> IMModel -> NoMessages
catchFile fileReader event model = CIF.nothingNext model $ liftEffect do
      CCF.readBase64 fileReader <<< WHEDT.files <<< WHED.dataTransfer <<< SU.fromJust $ WHED.fromEvent event
      CCD.preventStop event

setSelectedImage :: Maybe String -> IMModel -> NextMessage
setSelectedImage maybeBase64 model@{smallScreen} =
      model {
            toggleChatModal = ShowSelectedImage,
            selectedImage = maybeBase64,
            erroredFields =
                  if isTooLarge $ DM.fromMaybe "" maybeBase64 then
                        [TDS.reflectSymbol (Proxy :: Proxy "selectedImage")]
                   else
                        []
      } :> (if smallScreen then [] else [CIF.next $ FocusInput ImageFormCaption])
      where isTooLarge contents = maxImageSize < 3 * DI.ceil (DI.toNumber (DS.length contents) / 4.0)

toggleModal :: ShowChatModal -> IMModel -> MoreMessages
toggleModal toggle model = model {
      toggleChatModal = toggle,
      link = Nothing,
      selectedImage = Nothing,
      linkText = Nothing
} :>  case toggle of
            ShowSelectedImage -> [pickImage]
            ShowLinkForm -> [CIF.next $ FocusInput LinkFormUrl]
            ShowPreview -> [ setPreview ]
            _ -> []
      where pickImage = liftEffect do
                  fileInput <- CCD.unsafeGetElementByID ImageFileInput
                  CCF.triggerFileSelect fileInput
                  pure Nothing

            setPreview = liftEffect do
                  preview <- CCD.unsafeGetElementByID ChatInputPreview
                  input <- chatInput model
                  message <- CCD.value input
                  CCD.setInnerHTML preview $ SM.parse message
                  pure Nothing

setEmoji :: Event -> IMModel -> NextMessage
setEmoji event model =
      model :>
            if CCD.tagNameFromTarget event == "SPAN" then
                  [liftEffect do
                        emoji <- CCD.innerTextFromTarget event
                        input <- chatInput model
                        setAtCursor input emoji,
                  pure <<< Just $ ToggleChatModal HideChatModal
                  ]
            else
                  []

insertLink :: IMModel -> MoreMessages
insertLink model@{ linkText, link } =
      case link of
            Nothing -> F.noMessages $ model {
                  erroredFields = [ TDS.reflectSymbol (Proxy :: Proxy "link") ]
            }
            Just url ->
                  let { protocol } = NU.parse $ DS.trim url
                  in model {
                        erroredFields = []
                  } :> [
                        CIF.next $ ToggleChatModal HideChatModal,
                        insert $ if protocol == null then "http://" <> url else url
                  ]
      where markdown url = "[" <> DM.fromMaybe url linkText <> "](" <> url <> ")"
            insert text = liftEffect do
                  input <- chatInput model
                  setAtCursor input $ markdown text

setAtCursor :: Element ->  String -> Effect (Maybe IMMessage)
setAtCursor input text = do
      let textarea = SU.fromJust $ WHHTA.fromElement input
      end <- WHHTA.selectionEnd textarea
      value <- WHHTA.value textarea
      let { before, after } = DS.splitAt end value
      CIF.next <<< SetMessageContent (Just $ end + DS.length text + 1) $ before <> text <> after

resizeTextarea :: Element -> Effect Unit
resizeTextarea = EU.runEffectFn1 resizeTextarea_

resizeChatInput :: Event -> IMModel -> NextMessage
resizeChatInput event model = CIF.nothingNext model resize
      where resize = liftEffect <<< resizeTextarea <<< SU.fromJust $ do
                  target <- WEE.target event
                  WDE.fromEventTarget target

toggleMessageEnter :: IMModel -> NoMessages
toggleMessageEnter model@{ messageEnter } = F.noMessages $ model {
      messageEnter = not messageEnter
}