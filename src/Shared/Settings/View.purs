module Shared.Settings.View where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Data.Array ((:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.CodePoints as DSC
import Type.Proxy(Proxy(..))
import Data.Symbol (class IsSymbol)
import Flame (Html)
import Flame.Html.Attribute (ToSpecialEvent)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Type.Proxy(Proxy(..))
import Prim.Row (class Cons)
import Prim.Symbol (class Append)
import Record as R
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Path as SP
import Shared.Unsafe as SU
import Type.Data.Symbol as TDS
import Web.DOM.Element as WDE
import Web.Event.Event as WEE

view :: SettingsModel -> Html SettingsMessage
view model =
      HE.div (HA.class' "settings-edition") [
            HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.pathery CSS "settings.6390cea6e61528add6f3"],
            account model
      ]

formId :: forall field. IsSymbol field => Proxy field -> String
formId field = TDS.reflectSymbol field <> "-form"

account :: SettingsModel -> Html SettingsMessage
account model@{ erroredFields, confirmTermination } =
      HE.div (HA.class' "settings-section") [
            HE.div (HA.class' "section-label") [
                  HE.div (HA.class' "bold") "Account",
                  HE.div (HA.class' "duller") [
                        HE.text "Change your password, email",
                        HE.br,
                        HE.text "or close your account"
                  ]
            ],
            HE.div_ [
                  --REFACTOR: this be ugly
                  fieldConfirmationSection (Proxy :: Proxy "email") "text" emailMaxCharacters validateEmail "Please enter a valid email" ChangeEmail,

                  fieldConfirmationSection (Proxy :: Proxy "password") "password" passwordMaxCharacters validatePassword ("Password must be " <> show passwordMinCharacters <> " characters or more") ChangePassword,

                  HE.div (formId (Proxy :: Proxy "confirmTermination")) [
                        HE.label_ "Permanently delete all my data and close my account",
                        HE.div (HA.class' "section-buttons margined") [
                              HE.input [HA.type' "button", HA.value "Terminate account", HA.class' "green-button danger", HA.onClick ToggleTerminateAccount],
                              HE.span' (HA.class' "request-error-message"),
                              HE.span (HA.class' "success-message") [
                                    HE.text "Account terminated!",
                                    HE.br,
                                    HE.text "You will be logged out..."
                              ]
                        ],
                        HE.div (HA.class' {"modal-placeholder-overlay": true, hidden: not confirmTermination })[
                              HE.div (HA.class' "confirmation") [
                                    HE.span (HA.class' "bold") "Do you really want to terminate your account? All of your data will be permanently lost.",
                                    HE.div (HA.class' "buttons") [
                                          HE.button [HA.class' "cancel", HA.onClick ToggleTerminateAccount] "Cancel",
                                          HE.button [HA.class' "green-button danger", HA.onClick TerminateAccount] "Terminate"
                                    ]
                              ]
                        ]
                  ]
            ]
      ]
      where fieldConfirmationSection :: forall field fieldConfirmation r t. IsSymbol field => Cons field String r SM => Append field "Confirmation" fieldConfirmation => IsSymbol fieldConfirmation => Cons fieldConfirmation String t SM => Proxy field -> String -> Int -> (String -> Boolean) -> String -> SettingsMessage -> Html SettingsMessage
            fieldConfirmationSection field inputType maxChars validator fieldErrorMessage message =
                  let   stringField = TDS.reflectSymbol field
                        capitalizedStringField = capitalize stringField
                        fieldValue = R.get field model
                        fieldConfirmation = TDS.append field (Proxy :: Proxy "Confirmation")
                        stringFieldConfirmation = TDS.reflectSymbol fieldConfirmation
                        fieldConfirmationValue = R.get fieldConfirmation model
                        hasErrors = DA.elem stringField erroredFields
                        hasConfirmationErrors = DA.elem stringFieldConfirmation erroredFields
                        messageIfValidated =
                              if not $ validator fieldValue then
                                    setValidatedField (const false) field fieldValue
                               else if fieldValue /= fieldConfirmationValue then
                                    setValidatedField (const false) fieldConfirmation fieldConfirmationValue
                               else
                                    message
                  in HE.div (formId field) [
                        HE.div (HA.class' { errored: hasErrors }) [
                              HE.label_ capitalizedStringField ,
                              HE.input [HA.type' inputType, HA.class' "modal-input", HA.value fieldValue, onChangeValue (setValidatedField validator field), HA.autocomplete $ "new-" <> stringField],
                              HE.div (HA.class' "error-message") fieldErrorMessage
                        ],
                        HE.div (HA.class' { errored: hasConfirmationErrors }) [
                              HE.label_ $ "Confirm " <> stringField,
                              HE.input [HA.type' inputType, HA.autocomplete $ "new-" <> stringField, HA.class' "modal-input", HA.value fieldConfirmationValue, onChangeValue (setValidatedField (_ == fieldValue) fieldConfirmation)],
                              HE.div (HA.class' "error-message") $ capitalizedStringField <> " confirmation must match " <> stringField
                        ],
                        HE.div (HA.class' "section-buttons") [
                              HE.input [HA.type' "button", HA.class' "green-button", HA.disabled $ hasErrors || hasConfirmationErrors, HA.value $ "Change " <> stringField, HA.onClick messageIfValidated],
                              HE.span' (HA.class' "request-error-message"),
                              HE.span (HA.class' "success-message") [
                                    HE.text $ capitalizedStringField <> " changed!",
                                    HE.br,
                                    HE.text "You will be logged out..."
                              ]
                        ]
                  ]

onChangeValue :: forall message. ToSpecialEvent message String
onChangeValue constructor = HA.createRawEvent "change" handler
      where handler event = Just <<< constructor <$> CCD.value (extractElement event)
            extractElement event = SU.fromJust do
                  target <- WEE.target event
                  WDE.fromEventTarget target

validateEmail :: String -> Boolean
validateEmail email = DS.contains (Pattern "@") email && DS.contains (Pattern ".") email

validateEmailConfirmation :: String -> String -> Boolean
validateEmailConfirmation email confirmation = email == confirmation

validatePassword :: String -> Boolean
validatePassword password = DS.length password >= passwordMinCharacters

validatePasswordConfirmation :: String -> String -> Boolean
validatePasswordConfirmation  password confirmation = password == confirmation

setValidatedField :: forall field r. IsSymbol field => Cons field String r SM => (String -> Boolean) -> Proxy field -> String -> SettingsMessage
setValidatedField validator field value = SetSField validated
      where stringField = TDS.reflectSymbol field
            validated model =
                  R.set field value $ model {
                        erroredFields =
                              if validator value then
                                    DA.delete stringField model.erroredFields
                              else
                                    stringField : model.erroredFields
                  }

capitalize :: String -> String
capitalize str = case DS.uncons str of
      Just o -> (DS.toUpper $ DSC.singleton o.head) <> o.tail
      _ -> str