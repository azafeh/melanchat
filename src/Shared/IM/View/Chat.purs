module Shared.IM.View.Chat where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Control.Alt ((<|>))
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.HashMap as HS
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Symbol (class IsSymbol)
import Data.Symbol as TDS
import Type.Proxy(Proxy(..))
import Data.Tuple (Tuple(..))
import Effect.Unsafe as EU
import Shared.IM.Types
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Prim.Row (class Cons)
import Shared.Experiments.Impersonation (impersonations)
import Shared.IM.Emoji as SIE
import Shared.IM.Svg as SIS
import Shared.Keydown as SK
import Shared.Markdown as SM
import Shared.Options.File (maxImageSizeKB)
import Shared.Setter as SS

chat :: IMModel -> Html IMMessage
chat model@{ chatting } =
      HE.div [HA.class' {"send-box" : true, "hidden": DM.isNothing chatting }, SK.keyDownOn "Escape" (const $ ToggleChatModal HideChatModal)] [
            linkModal model,
            imageModal model,
            chatBarInput ChatInput model
      ]

--REFACTOR: replace sproxy usage with just SetField (all the fields are known!)
linkModal :: IMModel -> Html IMMessage
linkModal {toggleChatModal, linkText, link, erroredFields} =
      HE.div [HA.class' {"link-form modal-form": true, hidden: toggleChatModal /= ShowLinkForm }] [
            HE.label_ "Text",
            HE.input [HA.type' "text", HA.placeholder "optional title", HA.value $ DM.fromMaybe "" linkText, HA.onInput (setJust (Proxy :: Proxy "linkText"))],
            HE.label_ "Link",
            HE.input [HA.type' "text", HA.id $ show LinkFormUrl, HA.placeholder "http://", HA.value $ DM.fromMaybe "" link, HA.onInput (setJust (Proxy :: Proxy "link"))],
            HE.span [HA.class' {"error-message": true, "invisible": not (link /= Nothing && DA.elem (TDS.reflectSymbol (Proxy :: Proxy "link")) erroredFields) }] "Please enter a link",
            HE.div (HA.class' "buttons") [
                  HE.button [HA.class' "cancel", HA.onClick $ ToggleChatModal HideChatModal] "Cancel",
                  HE.button [HA.class' "green-button", HA.onClick InsertLink] "Insert"
            ]
      ]

imageModal :: IMModel -> Html IMMessage
imageModal {selectedImage, erroredFields} =
      HE.div [HA.class' { "image-form modal-form": true, hidden: DM.isNothing selectedImage }] [
            HE.div (HA.class' { "upload-div": true, hidden : not imageValidationFailed }) [
                  HE.input [HA.id $ show ImageFileInput, HA.type' "file", HA.value "", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp"],
                  HE.div (HA.class' "error-message") $ "Image is larger than the " <> maxImageSizeKB <> " limit. Please select a different file."
            ],
            HE.div (HA.class' { "image-form-image": true, hidden: imageValidationFailed }) [
                  HE.img <<< HA.src $ DM.fromMaybe "" selectedImage
            ],
            HE.div (HA.class' "image-form-controls") [
                  HE.label_ "Caption",
                  HE.input [HA.placeholder "optional title", HA.id $ show ImageFormCaption, HA.type' "text", HA.onInput (setJust (Proxy :: Proxy "imageCaption"))],
                  HE.div (HA.class' "image-buttons") [
                        HE.button [HA.class' "cancel", HA.onClick $ ToggleChatModal HideChatModal] "Cancel",
                        HE.svg [HA.class' "svg-50 send-image-button", HA.onClick ForceBeforeSendMessage, HA.viewBox "0 0 16 16"] $ sendButtonElements "Send file"
                  ]
            ]
      ]
      where imageValidationFailed = DA.elem (TDS.reflectSymbol (Proxy :: Proxy "selectedImage")) erroredFields

chatBarInput :: ElementID -> IMModel -> Html IMMessage
chatBarInput elementID model@{
      chatting,
      contacts,
      suggesting,
      isWebSocketConnected,
      messageEnter,
      suggestions,
      toggleChatModal
 } = HE.fragment [
      emojiModal model,
      HE.div (HA.class' { hidden: toggleChatModal /= ShowPreview})  [
            HE.div [HA.class' "chat-input-options"] [
                  HE.svg [HA.onClick $ ToggleChatModal HideChatModal, HA.class' "svg-32 boio-2", HA.viewBox "0 0 16 16" ] $ HE.title "Exit preview" : SIS.closeElements
            ],
            HE.div' [HA.id $ show ChatInputPreview, HA.class' "chat-input-preview message-content"]
      ],
      HE.div [HA.class' { hidden: not available || toggleChatModal == ShowPreview || DM.isNothing chatting && DM.isNothing suggesting }] [
            HE.div [HA.class' "chat-input-options"] [
                  bold,
                  italic,
                  strikethrough,
                  heading,
                  unorderedList,
                  orderedList,
                  linkButton toggleChatModal,
                  previewButton,
                  HE.div (HA.class' "send-enter") [
                        HE.input [HA.type' "checkbox", HA.autocomplete "off", HA.checked messageEnter, HA.onClick ToggleMessageEnter, HA.id "message-enter"],
                        HE.label (HA.for "message-enter") "Send message on enter"
                  ]
            ],
            HE.div [HA.class' {"chat-input-area" : true, side: not messageEnter}  ] [
                  emojiButton model,
                  HE.textarea' $ [
                        HA.rows 1,
                        HA.class' "chat-input",
                        HA.id $ show elementID,
                        HA.placeholder $ if isWebSocketConnected then "Type here to message " <> recipientName else "Waiting for connection...",
                        HA.disabled $ not isWebSocketConnected,
                        SK.keyDownOn "Enter" EnterBeforeSendMessage,
                        HA.onInput' ResizeChatInput,
                        HA.autocomplete "off"
                  ],
                  HE.div (HA.class' "chat-right-buttons") [
                        imageButton,
                        sendButton messageEnter
                  ]
            ]
      ]
]
      where available = DM.fromMaybe true $ getContact _.available
            recipientName = DM.fromMaybe "" $ impersonationName <|> getContact (_.name <<< _.user) <|> getProperty suggesting suggestions _.name

            impersonationName = do
                  index <- chatting
                  { impersonating } <- contacts !! index
                  id <- impersonating
                  imp <- HS.lookup id impersonations
                  pure $ imp.name

            getContact :: forall a. (Contact -> a) -> Maybe a
            getContact = getProperty chatting contacts

            getProperty :: forall a b. Maybe Int -> Array b -> (b -> a) -> Maybe a
            getProperty index list accessor = do
                  i <- index
                  entry <- list !! i
                  pure $ accessor entry

setJust :: forall t7 t8 t9. IsSymbol t8 => Cons t8 (Maybe t9) t7 IM => Proxy t8 -> t9 -> IMMessage
setJust field = SS.setIMField field <<< Just

bold :: Html IMMessage
bold = HE.svg [HA.class' "svg-20 boio", HA.onClick (Apply Bold), HA.viewBox "0 0 300 300"] [
      HE.title "Bold",
      HE.path' $ HA.d "M278,205.35q0,20.73-8.74,37a77.48,77.48,0,0,1-24,27.05,114.73,114.73,0,0,1-38.9,18q-21.21,5.38-53.75,5.37H22V7.16H138.13q36.18,0,53,2.3a112.71,112.71,0,0,1,33.24,10.17q17.11,8.27,25.44,22.15t8.33,31.76q0,20.73-11.46,36.54c-7.64,10.58-18.41,18.77-32.34,24.67v1.53q29.31,5.55,46.51,23T278,205.35ZM179.14,87.93a30.65,30.65,0,0,0-3.73-14.2q-3.71-7.11-13.16-10.55c-5.62-2-12.63-3.15-21-3.37s-20.15-.27-35.28-.27H98.75V120h12.06q18.28,0,31.13-.57t20.31-3.84q10.44-4.4,13.65-11.4A38.23,38.23,0,0,0,179.14,87.93ZM197.8,204.2c0-9.07-1.84-16.09-5.51-21s-9.92-8.61-18.75-11.05c-6-1.67-14.3-2.55-24.86-2.69s-21.59-.19-33.08-.19H98.75v71.18h5.61q32.49,0,46.52-.19a70.8,70.8,0,0,0,25.87-5q12-4.8,16.56-12.77A36.38,36.38,0,0,0,197.8,204.2Z"
]

italic :: Html IMMessage
italic = HE.svg [HA.class' "svg-20", HA.onClick (Apply Italic), HA.viewBox "0 0 300 300"] [
      HE.title "Italic",
      HE.path' $ HA.d "M172.19,278H65.92l5.94-24.93H107L157,46.93H121.87L127.81,22H234.08l-5.94,24.93H193L143,253.07h35.12Z"
]

strikethrough :: Html IMMessage
strikethrough = HE.svg [HA.class' "svg-20", HA.onClick (Apply Strike), HA.viewBox "0 0 300 300"] [
      HE.title "Strikethrough",
      HE.path' (HA.d "M205.24,196.59a55.62,55.62,0,0,1-6.19,25.09,59.94,59.94,0,0,1-17.33,21,93.84,93.84,0,0,1-28.48,14.58q-16.25,5.25-39.17,5.25a204.55,204.55,0,0,1-44.23-4.37,232.6,232.6,0,0,1-39.93-13V209h2.14a127.24,127.24,0,0,0,39.86,21.15q22.62,7.43,42.45,7.43,28.13,0,43.78-10.06t15.64-26.83q0-14.44-7.39-21.29t-22.55-10.65A235.09,235.09,0,0,0,119,163.93Q105.65,162,90.7,159.11,60.45,153,45.86,138.2T31.28,99.61q0-27.27,24.15-44.7t61.23-17.42a206.72,206.72,0,0,1,44,4.37,209,209,0,0,1,35.41,10.8V86.78H194q-13-10.5-34.15-17.42a138.13,138.13,0,0,0-43.29-6.93q-24.29,0-39,9.62T62.76,96.84q0,13.57,7.31,21.3T95.89,130q9.78,2,27.81,5t30.54,6q25.35,6.41,38.17,19.39T205.24,196.59Z"),
      HE.path' (HA.d "M22,185.93v-17.5H278v17.5Z")
]

heading :: Html IMMessage
heading = HE.svg [HA.class' "svg-20", HA.onClick (Apply Heading), HA.viewBox "0 0 300 300"] [
      HE.title "Heading",
      HE.path' $ HA.d "M270.19,278H201.05V165.22H99V278H29.81V22H99v93.7h102.1V22h69.14Z"
]

unorderedList :: Html IMMessage
unorderedList = HE.svg [HA.class' "svg-20", HA.onClick (Apply UnorderedList), HA.viewBox "0 0 16 16" ] [
      HE.title "Bulleted list",
      HE.rect' [HA.class' "strokeless", HA.x "5", HA.y "13.14", HA.width "11", HA.height "1"],
      HE.rect' [HA.class' "strokeless", HA.x "5", HA.y "7.5", HA.width "11", HA.height "1"],
      HE.rect' [HA.class' "strokeless", HA.x "5", HA.y "2.05", HA.width "11", HA.height "1"],
      HE.circle' [HA.class' "strokeless", HA.cx "2", HA.cy "2.55", HA.r "2"],
      HE.circle' [HA.class' "strokeless", HA.cx "2", HA.cy "8", HA.r "2"],
      HE.circle' [HA.class' "strokeless", HA.cx "2", HA.cy "13.64", HA.r "2"]
]

orderedList :: Html IMMessage
orderedList = HE.svg [HA.class' "svg-20", HA.onClick (Apply OrderedList), HA.viewBox "0 0 16 16" ] [
      HE.title "Ordered list",
      HE.rect' [HA.class' "strokeless", HA.x "4.94", HA.y "13.04", HA.width "11.06", HA.height "1"],
      HE.rect' [HA.class' "strokeless", HA.x "4.94", HA.y "7.5", HA.width "11.06", HA.height "1"],
      HE.rect' [HA.class' "strokeless", HA.x "4.94", HA.y "1.95", HA.width "11.06", HA.height "1"],
      HE.rect' [HA.class' "strokeless", HA.x "1.39", HA.y "0.68", HA.width "0.71", HA.height "0.71"],
      HE.rect' [HA.class' "strokeless", HA.x "2.11", HA.width "0.75", HA.height "4.17"],
      HE.rect' [HA.class' "strokeless", HA.x "1.4", HA.y "4.18", HA.width "2.16", HA.height "0.76"],
      HE.polygon' [HA.class' "strokeless", HA.points "1.76 9.77 1.76 9.06 1.05 9.06 1.05 10.48 1.76 10.48 1.76 10.48 3.92 10.48 3.92 9.77 1.76 9.77"],
      HE.polygon' [HA.class' "strokeless", HA.points "1.76 6.23 3.18 6.23 3.18 7.64 3.89 7.64 3.89 6.23 3.18 6.23 3.18 5.52 1.76 5.52 1.76 6.22 1.05 6.22 1.05 6.93 1.76 6.93 1.76 6.23"],
      HE.rect' [HA.class' "strokeless", HA.x "2.47", HA.y "7.65", HA.width "0.71", HA.height "0.71"],
      HE.rect' [HA.class' "strokeless", HA.x "1.76", HA.y "8.36", HA.width "0.71", HA.height "0.71"],
      HE.rect' [HA.class' "strokeless", HA.x "1.07", HA.y "11.73", HA.width "0.72", HA.height "0.72"],
      HE.rect' [HA.class' "strokeless", HA.x "1.78", HA.y "11.02", HA.width "1.43", HA.height "0.72"],
      HE.polygon' [HA.class' "strokeless" ,HA.points "1.78 14.56 1.07 14.56 1.07 15.28 1.78 15.28 1.78 15.28 1.78 16 3.21 16 3.21 15.28 1.78 15.28 1.78 14.56"],
      HE.polygon' [HA.class' "strokeless", HA.points "3.92 11.74 3.2 11.74 3.2 13.16 1.78 13.16 1.78 13.87 3.2 13.87 3.2 15.28 3.92 15.28 3.92 13.85 3.21 13.85 3.21 13.17 3.92 13.17 3.92 11.74"]
]

linkButton :: ShowChatModal -> Html IMMessage
linkButton toggle = HE.svg [HA.class' "svg-20 link-button", HA.onClick <<< ToggleChatModal $ if toggle == ShowLinkForm then HideChatModal else ShowLinkForm, HA.viewBox "0 0 16 16"] [
      HE.title "Add link",
      HE.path' [HA.class' "strokeless", HA.d "M15.12,6l-.21-.2-.12-.09a4,4,0,0,0-.49-.3,2.79,2.79,0,0,0-.55-.2,1.5,1.5,0,0,0-.29-.06,2.32,2.32,0,0,0-.46,0H10a3.81,3.81,0,0,1,.75.75H13a2.26,2.26,0,0,1,0,4.52H9a2.11,2.11,0,0,1-1-.24,2.31,2.31,0,0,1-.7-.54,2.28,2.28,0,0,1,0-2.95H6.39l0,0h0a2.87,2.87,0,0,0-.18.4,2.73,2.73,0,0,0-.18,1,3.5,3.5,0,0,0,.18,1,3.52,3.52,0,0,0,.18.4h0a.25.25,0,0,1,0,.07l.19.3.19.21.1.11.14.13.18.16.24.16.26.14.29.12H8l.26.08a1.36,1.36,0,0,0,.29.06,3.42,3.42,0,0,0,.46,0h4a3.42,3.42,0,0,0,.46,0,1.5,1.5,0,0,0,.29-.06,3.62,3.62,0,0,0,.55-.2,4,4,0,0,0,.49-.3l.12-.09.21-.2a3,3,0,0,0,0-4.25Z"],
      HE.path' [HA.class' "strokeless", HA.d "M5.31,10.35H3A2.26,2.26,0,0,1,3,5.83H7a2.24,2.24,0,0,1,1,.24,2.34,2.34,0,0,1,.7.55,2.25,2.25,0,0,1,.54,1.47,2.59,2.59,0,0,1-.42,1.48h.83a7.09,7.09,0,0,0,.25-.73L10,8.55a3.44,3.44,0,0,0,0-.46l0-.46L9.9,7.34S9.8,7.06,9.76,7s-.1-.25-.12-.28a.93.93,0,0,0-.05-.1c-.06-.1-.12-.2-.19-.3l-.09-.11L9.11,6A1.85,1.85,0,0,0,9,5.83H9l-.18-.15a4.14,4.14,0,0,0-.5-.3A2.52,2.52,0,0,0,8,5.26H8l-.26-.08a1.36,1.36,0,0,0-.29-.06,2.32,2.32,0,0,0-.46,0H3a2.32,2.32,0,0,0-.46,0,1.5,1.5,0,0,0-.29.06,2.58,2.58,0,0,0-.55.2,4,4,0,0,0-.49.3l-.12.09a2.59,2.59,0,0,0-.4.41L.6,6.29a2.88,2.88,0,0,0-.3.5,2.6,2.6,0,0,0-.21.55L0,7.63a3.55,3.55,0,0,0,0,.46l0,.46.06.29L.3,9.4a2.48,2.48,0,0,0,.3.49L.69,10l.19.21.21.2.12.09a4,4,0,0,0,.49.3,3.27,3.27,0,0,0,.55.2,1.5,1.5,0,0,0,.29.06,3.42,3.42,0,0,0,.46,0H6.06A3.6,3.6,0,0,1,5.31,10.35Z"]
]

emojiButton :: IMModel -> Html IMMessage
emojiButton model@{toggleChatModal, smallScreen}
      | toggleChatModal == ShowEmojis =
            HE.div (HA.class' "emoji-access-div") $ HE.svg [HA.onClick $ ToggleChatModal HideChatModal, HA.class' "emoji-access", HA.viewBox "0 0 16 16"] $
                  if smallScreen then [
                        HE.path' [HA.class' "strokeless", HA.d "M14.9,2.24H1.1A1.11,1.11,0,0,0,0,3.35v9.18a1.1,1.1,0,0,0,1.1,1.1H14.9a1.1,1.1,0,0,0,1.1-1.1V3.35A1.11,1.11,0,0,0,14.9,2.24ZM15,12.53a.11.11,0,0,1-.1.1H1.1a.11.11,0,0,1-.1-.1V3.35a.11.11,0,0,1,.1-.11H14.9a.11.11,0,0,1,.1.11Z"],
                        HE.rect' [HA.class' "strokeless", HA.x "2.75", HA.y "4.53", HA.width "1.28", HA.height "1.28"],
                        HE.rect' [HA.class' "strokeless", HA.x "2.75", HA.y "7.3", HA.width "1.28", HA.height "1.28"],
                        HE.rect' [HA.class' "strokeless", HA.x "5.82", HA.y "4.53", HA.width "1.28", HA.height "1.28"],
                        HE.rect' [HA.class' "strokeless", HA.x "5.82", HA.y "7.3", HA.width "1.28", HA.height "1.28"],
                        HE.rect' [HA.class' "strokeless", HA.x "8.9", HA.y "4.53", HA.width "1.28", HA.height "1.28"],
                        HE.rect' [HA.class' "strokeless", HA.x "8.9", HA.y "7.3", HA.width "1.28", HA.height "1.28"],
                        HE.rect' [HA.class' "strokeless", HA.x "11.98", HA.y "4.53", HA.width "1.28", HA.height "1.28"],
                        HE.rect' [HA.class' "strokeless", HA.x "11.98", HA.y "7.3", HA.width "1.28", HA.height "1.28"],
                        HE.rect' [HA.class' "strokeless", HA.x "4.49", HA.y "10.18", HA.width "7.02", HA.height "1.28"]
                  ]
                  else
                        HE.title "Close emojis" : SIS.closeElements
      | otherwise =
           HE.div (HA.class' "emoji-access-div") $ HE.svg [HA.onClick $ ToggleChatModal ShowEmojis, HA.class' "emoji-access", HA.viewBox "0 0 16 16"] [
                  HE.title "Emojis",
                  HE.path' [HA.class' "strokeless", HA.d "M5.16,7.53a.71.71,0,1,0-.66-.71A.69.69,0,0,0,5.16,7.53Z"],
                  HE.path' [HA.class' "strokeless", HA.d "M10.8,7.53a.71.71,0,1,0-.66-.71A.68.68,0,0,0,10.8,7.53Z"],
                  HE.path' [HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0Zm0,15.5A7.5,7.5,0,1,1,15.47,8,7.51,7.51,0,0,1,8,15.45Z"],HE.path' [HA.class' "strokeless", HA.d "M2.34,9.19l0,.15a5.77,5.77,0,0,0,11.19,0l0-.15ZM8,13.45a5.47,5.47,0,0,1-5.31-4H13.27A5.49,5.49,0,0,1,8,13.45Z"]
            ]

imageButton :: Html IMMessage
imageButton = HE.svg [HA.onClick $ ToggleChatModal ShowSelectedImage, HA.class' "attachment-button", HA.viewBox "0 0 16 16"] [
      HE.path' [HA.class' "strokeless", HA.d "M10.91,4v8.78a2.44,2.44,0,0,1-.72,1.65A3.31,3.31,0,0,1,8,15.25H7.67a2.67,2.67,0,0,1-2.58-2.48L5.26,2.9V2.82l0-.2h0a2,2,0,0,1,.19-.7v0a1.82,1.82,0,0,1,1.6-1A1.69,1.69,0,0,1,7.73,1,2.14,2.14,0,0,1,9.16,2.81h0v7.81c0,.75-.36,1.26-1.13,1.26A1.12,1.12,0,0,1,6.9,10.63V4H6.11v6.61a1.93,1.93,0,0,0,2,2,1.83,1.83,0,0,0,1.82-2l0-7.81c0-.06,0-.12,0-.18s0-.11,0-.17,0,0,0-.05a2.59,2.59,0,0,0-.32-1s0,0,0,0A3.19,3.19,0,0,0,7.77.09h0A2.41,2.41,0,0,0,7.09,0a2.56,2.56,0,0,0-1,.21H6A2.74,2.74,0,0,0,4.76,1.39h0a3,3,0,0,0-.37,1.43v10A3.41,3.41,0,0,0,7.67,16H8A4,4,0,0,0,10.69,15a3.22,3.22,0,0,0,.93-2.18V4Z"]
]

sendButton :: Boolean -> Html IMMessage
sendButton messageEnter = HE.div [HA.class' {"send-button-div": true, hidden: messageEnter }, HA.onClick ForceBeforeSendMessage] $ HE.svg [HA.class' "send-button", HA.viewBox "0 0 16 16"] $ sendButtonElements "Send message"

sendButtonElements :: String -> Array (Html IMMessage)
sendButtonElements title = [
      HE.title title,
      HE.path' [HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM8,15.25A7.25,7.25,0,1,1,15.25,8,7.26,7.26,0,0,1,8,15.25Z"],
      HE.polygon' [HA.class' "strokeless", HA.points "4.02 4.02 6.06 7.99 4.02 12.01 13.63 8 4.02 4.02"]
]

previewButton :: Html IMMessage
previewButton = HE.svg [HA.class' "svg-20", HA.onClick $ ToggleChatModal ShowPreview, HA.viewBox "0 0 16 16" ][
      HE.title "Preview",
      HE.path' [HA.class' "strokeless", HA.d "M1,11.05V3.12a0,0,0,0,1,0,0H12.43V6l1,.63v-2L16,2.07H.5A.51.51,0,0,0,0,2.6v8.85c0,.29.22.62.5.62H6.58l-.71-1Z"],
      HE.rect' [HA.class' "strokeless", HA.x "2.42", HA.y "7.02", HA.width "3.99", HA.height "1.02"],
      HE.rect' [HA.class' "strokeless", HA.x "2.42", HA.y "5.11", HA.width "5.4", HA.height "0.98"],
      HE.path' [HA.class' "strokeless", HA.d "M10.26,8.36a1.37,1.37,0,1,0,1.37,1.37A1.37,1.37,0,0,0,10.26,8.36Z"],
      HE.path' [HA.class' "strokeless", HA.d "M15.09,13.55h0l-.54-.48L13,11.66a3.34,3.34,0,0,0-2.73-5.27A3.35,3.35,0,0,0,7.11,8.58a3.51,3.51,0,0,0-.19,1.15,3.35,3.35,0,0,0,3.34,3.35,3.31,3.31,0,0,0,2.2-.85l1.48,1.33.55.5.83.75a.42.42,0,0,0,.28.1.43.43,0,0,0,.28-.11h0a.36.36,0,0,0,0-.53Zm-4.83-1.22a2.6,2.6,0,0,1-2.59-2.6,3,3,0,0,1,.14-.89A2.69,2.69,0,0,1,9,7.48a2.53,2.53,0,0,1,1.28-.34,2.6,2.6,0,0,1,0,5.19Z"]
]

emojiModal  :: IMModel -> Html IMMessage
emojiModal { toggleChatModal } = HE.div [HA.class' { "emoji-wrapper": true, hidden: toggleChatModal /= ShowEmojis }] <<< HE.div [HA.class' "emojis", HA.onClick' SetEmoji] $ map toEmojiCategory SIE.byCategory
      where toEmojiCategory (Tuple name pairs) = HE.div_ [
                  HE.div (HA.class' "duller") name,
                  HE.div_ $ map (HE.span_ <<< _.s) pairs
            ]