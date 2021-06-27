module Shared.Profile.View where

import Prelude
import Shared.Types

import Data.Array ((:))
import Data.Array as DA
import Data.Foldable as DF
import Data.HashMap as DH
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Shared.Experiments.Types
import Data.Newtype as DN
import Data.String as DS
import Data.String.Read as DSR
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Shared.IM.Types
import Data.Tuple as DT
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Prim.Row (class Cons)
import Type.Proxy (Proxy(..))
import Prim.Symbol (class Append)
import Record as R
import Shared.Avatar as SA
import Shared.DateTime as SDT
import Shared.IM.Svg as SIS
import Shared.IM.View.Profile as SIVP
import Shared.Markdown as SM
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, maxLanguages, maxTags, nameMaxCharacters, tagMaxCharacters)
import Shared.Path as SP
import Shared.Setter as SS
import Shared.Unsafe as SU
import Type.Data.Symbol as TDS

profileEditionId :: String
profileEditionId = "profile-edition-form"

--REFACTOR: some bits can still be abstracted
view :: ProfileModel -> Html ProfileMessage
view model@{
      user,
      countries,
      languages,
      generating,
      hideSuccessMessage,
      descriptionInputed,
      experimenting
} = HE.div profileEditionId [
      impersonationProfile experimenting,

      HE.div [HA.class' {"profile-edition suggestion contact": true, hidden: DM.isJust experimenting}] [
            HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href $ SP.pathery CSS "profile.ac60b6d1cf56812f13e6"],
            HE.div (HA.class' "avatar-edition") [
                  HE.div (HA.onClick SelectAvatar) [
                        HE.img [HA.class' "avatar-profile-edition", HA.src $ SA.avatarForSender user.avatar],
                        pen
                  ],
                  HE.svg [HA.class' "svg-16", HA.viewBox "0 0 16 16", HA.onClick resetAvatar] $
                        HE.title "Reset profile picture" : removeContents
            ],
            HE.input [HA.id "avatar-file-input", HA.type' "file", HA.class' "hidden", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp"],

            displayEditName,
            displayEditHeadline,

            HE.div (HA.class' "profile-karma") [
                  HE.div_ [
                        HE.span [HA.class' "span-info"] $ show user.karma,
                        HE.span [HA.class' "duller"] " karma"
                  ]
            ],

            HE.div (HA.class' "profile-asl") [
                  displayEditAge,
                  displayEditGender,
                  displayEditCountry,
                  displayEditLanguages
            ],

            displayEditTags,
            displayEditDescription,

            HE.div (HA.class' "button-save-bottom") [
                  HE.input [HA.type' "button", HA.onClick SaveProfile, HA.value "Update profile", HA.class' "green-button center-flex"],
                  HE.span' (HA.class' "request-error-message"),
                  HE.span [HA.class' {"success-message": true, hidden: hideSuccessMessage } ] "Profiled updated!"
            ]
      ]
]
      where displayEditName = displayEditGenerated Name (Proxy :: Proxy "name") "This is the name other users will see when looking at your profile" nameMaxCharacters
            displayEditHeadline = displayEditGenerated Headline (Proxy :: Proxy "headline") "A tagline to draw attention to your profile" headlineMaxCharacters

            displayEditAge =
                  let   field = Proxy :: Proxy "age"
                        fieldInputed = Proxy :: Proxy "ageInputed"
                        currentFieldValue = map show <<< SDT.ageFrom $ map DN.unwrap user.age
                        parser = map DateWrapper <<< SDT.unformatISODate
                        control = HE.input [
                              HA.type' "date",
                              HA.class' "modal-input",
                              HA.placeholder "yyyy-mm-dd",
                              HA.onInput (setFieldInputed fieldInputed <<< parser),
                              HA.value <<< DM.fromMaybe "" $ map SDT.formatISODate user.age
                        ]
                  in displayEditOptionalField field (map HE.span_ currentFieldValue) control
            displayEditGender = displayEditOptional DSR.read genders (Proxy :: Proxy "gender") (HE.span_ <<< show <$> user.gender)
            displayEditCountry =
                  let display country = HE.div (HA.class' "blocky") [
                        HE.span (HA.class' "duller") "from ",
                        HE.text country
                  ]
                  in displayEditOptional DI.fromString countries (Proxy :: Proxy "country") do
                        country <- user.country
                        display <<< DT.snd <$> DF.find ((country == _) <<< DT.fst) countries

            displayEditLanguages =
                  let   fieldInputed = Proxy :: Proxy "languagesInputed"
                        currentFieldValue = case map getLanguage model.user.languages of
                              [] -> Nothing
                              languages -> Just $ HE.div (HA.class' "blocky") [
                                    HE.span (HA.class' "duller") "speaks ",
                                    HE.text $ DS.joinWith ", " languages
                              ]
                        control = HE.select [HA.onInput (setFieldInputedMaybe fieldInputed <<< DI.fromString)] $ displayOptionsWith "Select" model.languagesInputed languages
                  in  displayEditList (Proxy :: Proxy "languages") getLanguage currentFieldValue control ("You may select up to " <> show maxLanguages <> " four languages") maxLanguages
            displayEditTags =
                  let   fieldInputed = Proxy :: Proxy "tagsInputed"
                        fieldInputedList = Proxy :: Proxy "tagsInputedList"
                        currentFieldValue = case model.user.tags of
                              [] -> Nothing
                              tags -> Just $ HE.div (HA.class' "blocky") $ map (HE.span (HA.class' "tag")) tags
                        control = HE.input [
                              HA.type' "text",
                              HA.class' "modal-input",
                              -- SF.focus,
                              HA.maxlength tagMaxCharacters,
                              HA.value $ DM.fromMaybe "" model.tagsInputed,
                              HA.onKeydown (exitEditGenerated (appendInputedMaybe fieldInputedList fieldInputed) fieldInputed),
                              HA.onInput (setFieldInputedMaybe fieldInputed <<< nothingOnEmpty)
                        ]
                  in  HE.div (HA.class' "profile-tags") $ displayEditList (Proxy :: Proxy "tags") identity currentFieldValue control ("You may add up to " <> show maxTags <> " tags to show your interests, hobbies, etc") maxTags

            displayEditDescription = HE.fragment [
                  HE.div (HA.class' { hidden: generating /= Just Description })[
                        HE.div' (HA.class' "loading")
                  ],
                  HE.div (HA.class' {invisible: generating == Just Description}) [
                        HE.div [title "description", HA.class' {hidden: DM.isJust descriptionInputed}, HA.onClick (editField (Proxy :: Proxy "description") (Proxy :: Proxy "descriptionInputed"))] [
                              HE.div (HA.class' "about") [
                                    HE.span (HA.class' "duller") "About",
                                    pen
                              ],
                              HE.div' [HA.class' "profile-description", HA.innerHtml $ SM.parse user.description]
                        ],
                        HE.div [title "description", HA.class' {"description-edition": true, hidden: DM.isNothing descriptionInputed}] [
                              HE.div (HA.class' "bold") "Your description",
                              HE.div (HA.class' "duller") [
                                    HE.text "Talk a little (or a lot) about yourself",
                                    HE.br,
                                    HE.text "Leave it blank to generate a new random description"
                              ],
                              HE.textarea [
                                    HA.class' "profile-edition-description",
                                    HA.maxlength descriptionMaxCharacters,
                                   -- SF.focus,
                                    HA.onInput (setFieldInputed (Proxy :: Proxy "descriptionInputed"))
                              ] $ DM.fromMaybe "" descriptionInputed,

                              HE.div (HA.class' "save-cancel") [
                                    check (SetGenerate Description),
                                    cancel (Proxy :: Proxy "descriptionInputed")
                              ]
                        ]
                  ]
            ]

            displayEditList :: forall r s t u field fieldInputed fieldInputedList. IsSymbol field => Append field "Inputed" fieldInputed => IsSymbol fieldInputed => Append field "InputedList" fieldInputedList => IsSymbol fieldInputedList => Cons fieldInputed (Maybe t) u PM => Cons fieldInputedList (Maybe (Array t)) r PM => Cons field (Array t) s PU => Ord t => Eq t => Proxy field -> (t -> String) -> Maybe (Html ProfileMessage) -> Html ProfileMessage -> String -> Int -> Html ProfileMessage
            displayEditList field formatter currentFieldValue control explanation maxElements =
                  let   stringField = TDS.reflectSymbol field
                        fieldInputed = TDS.append field (Proxy :: Proxy "Inputed")
                        fieldInputedList = TDS.append field (Proxy :: Proxy "InputedList")
                        currentFieldInputedList = R.get fieldInputedList model
                        isEditing = DM.isJust $ currentFieldInputedList
                        displayRemoveItem item =
                              HE.div_ [
                                    HE.span (HA.class' $ "list-" <> stringField) $ formatter item,
                                    HE.svg [HA.class' "list-remove svg-20", HA.viewBox "0 0 16 16", HA.onClick (removeFromInputedList fieldInputedList item)] $ HE.title "Remove item" : removeContents
                              ]
                  in HE.div (HA.class' { centered: isEditing }) [
                        HE.div [HA.class' { "profile-edition-add": true, hidden: isEditing }, title stringField, HA.onClick (editField field fieldInputedList)] $
                              case currentFieldValue of
                                    Nothing -> HE.div (HA.class' "value-edition italics") [
                                          HE.text $ "Your " <> stringField,
                                          pen
                                    ]
                                    Just value -> HE.div (HA.class' "value-edition") [
                                          value,
                                          pen
                                    ],
                        HE.div (HA.class' { edition: true, hidden: not isEditing }) [
                              HE.div (HA.class' "bold") $ "Your " <> stringField,
                              HE.span (HA.class' "duller") explanation,
                              HE.br,
                              HE.div_ [
                                    control,
                                    plus (DA.length (DM.fromMaybe [] currentFieldInputedList) == maxElements) (appendInputedMaybe fieldInputedList fieldInputed)
                              ],
                              HE.div (HA.class' "profile-edition-add-list") [
                                    HE.div (HA.class' "grow") <<< map displayRemoveItem $ DM.fromMaybe [] currentFieldInputedList,
                                    HE.div_ [
                                          check (copyToField field fieldInputedList),
                                          cancel fieldInputedList
                                    ]
                              ]
                        ]
                  ]

            displayEditOptional :: forall r s t field fieldInputed. IsSymbol field => Append field "Inputed" fieldInputed => IsSymbol fieldInputed => Cons fieldInputed (Choice (Maybe t)) r PM => Cons field (Maybe t) s PU => Show t => Eq t => (String -> Maybe t) -> Array (Tuple t String) -> Proxy field -> Maybe (Html ProfileMessage) -> Html ProfileMessage
            displayEditOptional parser options field currentFieldValue =
                  let   fieldInputed = TDS.append field (Proxy :: Proxy "Inputed")
                  in displayEditOptionalField field currentFieldValue $ HE.select [HA.onInput (setFieldInputed fieldInputed <<< parser)] $ displayOptions (R.get field model.user) options

            displayEditOptionalField :: forall r s t field fieldInputed. IsSymbol field => Append field "Inputed" fieldInputed => IsSymbol fieldInputed => Cons fieldInputed (Choice (Maybe t)) r PM => Cons field (Maybe t) s PU => Proxy field -> Maybe (Html ProfileMessage) -> Html ProfileMessage -> Html ProfileMessage
            displayEditOptionalField field currentFieldValue control =
                  let   stringField = TDS.reflectSymbol field
                        fieldInputed = TDS.append field (Proxy :: Proxy "Inputed")
                        isEditing = DM.isJust $ R.get fieldInputed model
                  in HE.div (HA.class' { centered: isEditing }) [
                        HE.div [HA.class' { "profile-edition-add" : true, hidden: isEditing }, title stringField, HA.onClick (editField field fieldInputed)] $
                              case currentFieldValue of
                                    Just value -> HE.div (HA.class' "value-edition") [
                                          value,
                                          pen
                                    ]
                                    _ -> HE.div (HA.class' "italics value-edition") [
                                          HE.text $ "Your " <> stringField,
                                          pen
                                    ],
                        HE.div (HA.class' { edition: true, hidden: not isEditing }) [
                              HE.div (HA.class' "bold") $ "Your " <> stringField,
                              HE.div_ [
                                    control,
                                    check (copyFromChoice field fieldInputed),
                                    cancel fieldInputed
                              ]
                        ]
                  ]

            displayEditGenerated :: forall r s field fieldInputed. IsSymbol field => Append field "Inputed" fieldInputed => IsSymbol fieldInputed => Cons fieldInputed (Maybe String) r PM => Cons field String s PU => Generate -> Proxy field -> String -> Int -> Html ProfileMessage
            displayEditGenerated what field explanation maxLength =
                  let   stringField = TDS.reflectSymbol field
                        fieldInputed = TDS.append field (Proxy :: Proxy "Inputed")
                        currentInputed = R.get fieldInputed model
                        isEditing = DM.isJust currentInputed
                  in if generating == Just what then
                        HE.div' (HA.class' "loading")
                      else
                        HE.div_ [
                              HE.div [title stringField, HA.class' {"hidden": isEditing, "value-edition" : true}, HA.onClick (editField field fieldInputed)] [
                                    HE.span [HA.class' $ "profile-edition-" <> stringField] [HE.text $ R.get field model.user],
                                    pen
                              ],
                              HE.div (HA.class' { edition: true, hidden: not isEditing}) [
                                    HE.div (HA.class' "bold") $ "Your " <> stringField,
                                    HE.div (HA.class' "duller") [
                                          HE.text explanation,
                                          HE.br,
                                          HE.text $ "Leave it blank to generate a new random " <> stringField
                                    ],
                                    HE.div_ [
                                          HE.input [
                                                -- SF.focus,
                                                HA.class' "modal-input",
                                                HA.maxlength maxLength,
                                                HA.onKeydown (exitEditGenerated (SetGenerate what) fieldInputed),
                                                HA.onInput (setFieldInputed fieldInputed),
                                                HA.value $ DM.fromMaybe "" currentInputed
                                          ],
                                          check (SetGenerate what),
                                          cancel fieldInputed
                                    ]
                              ]
                        ]

            languageHM = DH.fromArray $ map (\{id, name} -> Tuple id name) languages
            getLanguage = SU.fromJust <<< flip DH.lookup languageHM

--refactor: abstract with shared/profile
impersonationProfile :: Maybe ExperimentData -> Html ProfileMessage
impersonationProfile = case  _ of
      Just (Impersonation (Just profile@{ name })) ->
            HE.div [HA.class' "suggestion old imper"] $ warning name : SIVP.displayUserProfile (Just 0) profile
      _ -> HE.createEmptyElement "div"
      where warning name = HE.div (HA.class' "imp impersonation-warning") [
                  HE.text "You are impersonating ",
                  HE.strong_ name,
                  HE.text ". Here is how your profile will be seen by other users:"
            ]

appendInputedMaybe :: forall t r u fieldInputedList fieldInputed. Ord t => IsSymbol fieldInputedList => Cons fieldInputedList (Maybe (Array t)) r PM => IsSymbol fieldInputed => Cons fieldInputed (Maybe t) u PM => Proxy fieldInputedList -> Proxy fieldInputed -> ProfileMessage
appendInputedMaybe fieldInputedList fieldInputed =
      SetPField $ \model ->
            case R.get fieldInputed model of
                  Nothing -> model
                  --refactor: this should be a set
                  Just value -> R.set fieldInputed Nothing $ R.set fieldInputedList (Just <<< DA.nub $ DA.snoc (DM.fromMaybe [] $ R.get fieldInputedList model) value) model

removeFromInputedList :: forall t r fieldInputedList. Eq t => IsSymbol fieldInputedList => Cons fieldInputedList (Maybe (Array t)) r PM => Proxy fieldInputedList -> t -> ProfileMessage
removeFromInputedList fieldInputedList value = SetPField (\model -> R.set fieldInputedList (map (DA.delete value) $ R.get fieldInputedList model) model)

editField :: forall s t r field fieldInputed. IsSymbol field => Cons field t s PU => IsSymbol fieldInputed => Cons fieldInputed (Maybe t) r PM => Proxy field -> Proxy fieldInputed -> ProfileMessage
editField field fieldInputed =
      SetPField (\model -> R.set fieldInputed (Just $ R.get field model.user) $ model {
            nameInputed = Nothing,
            headlineInputed = Nothing,
            ageInputed = Nothing,
            genderInputed = Nothing,
            countryInputed = Nothing,
            languagesInputed = Nothing,
            languagesInputedList = Nothing,
            tagsInputed = Nothing,
            tagsInputedList = Nothing,
            descriptionInputed = Nothing,
            generating = Nothing
      })

copyToField :: forall s t r field fieldInputed. IsSymbol field => Cons field (Array t) s PU => IsSymbol fieldInputed => Cons fieldInputed (Maybe (Array t)) r PM => Proxy field -> Proxy fieldInputed -> ProfileMessage
copyToField field fieldInputed = SetPField (\model -> R.set fieldInputed Nothing $ SS.setUserField field (DM.fromMaybe [] $ R.get fieldInputed model) model)

copyFromChoice :: forall s t r field fieldInputed. IsSymbol field => Cons field (Maybe t) s PU => IsSymbol fieldInputed => Cons fieldInputed (Choice (Maybe t)) r PM => Proxy field -> Proxy fieldInputed -> ProfileMessage
copyFromChoice field fieldInputed = SetPField (\model -> R.set fieldInputed Nothing $ SS.setUserField field (DM.fromMaybe Nothing $ R.get fieldInputed model) model)

resetFieldInputed :: forall fieldInputed r t.  IsSymbol fieldInputed => Cons fieldInputed (Maybe t) r PM => Proxy fieldInputed -> ProfileMessage
resetFieldInputed fieldInputed = SetPField (R.set fieldInputed Nothing)

setFieldInputed :: forall fieldInputed r t. IsSymbol fieldInputed => Cons fieldInputed (Maybe t) r PM => Proxy fieldInputed -> t -> ProfileMessage
setFieldInputed fieldInputed = setFieldInputedMaybe fieldInputed <<< Just

setFieldInputedMaybe :: forall fieldInputed r t. IsSymbol fieldInputed => Cons fieldInputed (Maybe t) r PM => Proxy fieldInputed -> Maybe t -> ProfileMessage
setFieldInputedMaybe fieldInputed v = SetPField (R.set fieldInputed v)

exitEditGenerated :: forall r field. IsSymbol field => Cons field (Maybe String) r PM => ProfileMessage -> Proxy field -> Tuple String String -> ProfileMessage
exitEditGenerated message fieldInputed (Tuple key _) =
      if key == "Enter" then
            message
       else if key == "Escape" then
            resetFieldInputed fieldInputed
       else
            SetPField identity

pen :: Html ProfileMessage
pen = HE.svg [HA.class' "svg-16 edit", HA.viewBox "0 0 16 16"] [
      HE.polygon' [HA.points "2.74 11.62 2.15 12.92 1.64 14.06 1.41 14.59 1.93 14.35 3.04 13.84 4.43 13.19 11.43 6.21 9.8 4.58 2.74 11.62"],
      HE.path' [HA.class' "strokeless", HA.d "M15.82,2.36,13.63.18A.56.56,0,0,0,13.21,0a.55.55,0,0,0-.41.18L10.66,2.31l3,3.06,2.14-2.13A.63.63,0,0,0,15.82,2.36Z"],
      HE.path' [HA.class' "strokeless", HA.d "M1.91,11l-.06.06-.61,1.41h0L.72,13.65.05,15.18A.58.58,0,0,0,.58,16a.6.6,0,0,0,.24-.06l1.52-.68,1.12-.52h0L5,14.08l7.9-7.87-3-3Zm2.52,2.15-.06,0L2.75,11.61l7.05-7,1.63,1.63Z"]
]

plus :: Boolean -> ProfileMessage -> Html ProfileMessage
plus isDisabled message = HE.svg attrs [
      HE.path' [HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM8,15.51A7.51,7.51,0,1,1,15.5,8,7.51,7.51,0,0,1,8,15.51Z"],
      HE.path' [ HA.d "M12.07,7.65a.5.5,0,0,0-.35-.14H8.49V4.3a.49.49,0,0,0-1,0V7.51H4.28a.5.5,0,0,0-.5.49.49.49,0,0,0,.15.36.5.5,0,0,0,.35.14H7.51v3.2a.45.45,0,0,0,.14.35A.47.47,0,0,0,8,12.2a.49.49,0,0,0,.49-.49V8.5h3.23a.5.5,0,0,0,.5-.49.49.49,0,0,0-.15-.36Z"]
]
      where attrs = [HA.class' {"svg-20 plus": true, disabled: isDisabled}, HA.viewBox "0 0 16 16"] <> if isDisabled then [] else [HA.onClick message]

check :: ProfileMessage -> Html ProfileMessage
check message = HE.svg [HA.class' "svg-20 save", HA.viewBox "0 0 16 16", HA.onClick message] [
      HE.title "Save edit",
      HE.path' [HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM8,15.5A7.5,7.5,0,1,1,15.5,8,7.5,7.5,0,0,1,8,15.5Z"],
      HE.path' [HA.d "M10.67,5.11l-4.3,4.3L4.73,7.77a.62.62,0,0,0-.88.88l2.52,2.52L11.55,6a.62.62,0,0,0-.88-.88Z"]
]

cancel :: forall r t fieldInputed. IsSymbol fieldInputed => Cons fieldInputed (Maybe t) r PM => Proxy fieldInputed -> Html ProfileMessage
cancel fieldInputed = HE.svg [HA.class' "svg-16 cancel", HA.viewBox "0 0 16 16", HA.onClick (resetFieldInputed fieldInputed) ] $
      HE.title "Cancel edit" : SIS.closeElements

displayOptions :: forall id. Show id => Eq id => Maybe id -> Array (Tuple id String) -> Array (Html ProfileMessage)
displayOptions = displayOptionsWith "Don't show"

displayOptionsWith :: forall i. Show i => Eq i => String -> Maybe i -> Array {id:: i, name :: String} -> Array (Html ProfileMessage)
displayOptionsWith unselectedText current = HE.option [HA.selected $ DM.isNothing current] <<< (unselectedText : _) <<< map makeOptions
      where makeOptions {id, name} = HE.option [HA.value $ show id, HA.selected $ Just id == current] name

title :: String -> NodeData ProfileMessage
title name = HA.title $ "Click to edit your " <> name

genders :: Array (Tuple Gender String)
genders = [Tuple Female $ show Female, Tuple Male $ show Male, Tuple NonBinary $ show NonBinary, Tuple Other $ show Other]

nothingOnEmpty :: String -> Maybe String
nothingOnEmpty s =
      case DS.trim s of
            "" -> Nothing
            v -> Just v

resetAvatar :: ProfileMessage
resetAvatar = SetPField $ \model -> model {
      user = model.user {
            avatar = Nothing
      }
}

removeContents :: forall message. Array (Html message)
removeContents = [
      HE.path' [HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM8,15.5A7.5,7.5,0,1,1,15.5,8,7.5,7.5,0,0,1,8,15.5Z"],
      HE.path' [HA.class' "a", HA.d "M11.65,7.38H4.4a.62.62,0,1,0,0,1.24h7.25a.62.62,0,0,0,0-1.24Z"]
]