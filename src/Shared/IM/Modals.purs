module Shared.IM.View.Modals where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Type.Proxy(Proxy(..))

import Data.Symbol as TDS
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.IM.Types
import Shared.IM.Svg as SIA
import Shared.IM.View.Retry as SIVR

modals :: IMModel -> Html IMMessage
modals { toggleModal: toggle, failedRequests, erroredFields } =
      HE.div (HA.class' {"modal-placeholder-overlay": true, "hidden" : toggle == HideUserMenuModal}) [
            case toggle of
                  ShowReport reportedID ->
                        HE.div (HA.class' "confirmation report") [
                              HE.span (HA.class' "report-title") "Report user",
                              HE.div (HA.class' "report-reasons") $ DA.mapWithIndex toRadio [DatingContent, Harrassment, HateSpeech, Spam, OtherReason],
                              HE.span [HA.class' {"error-message": true, "invisible": not (DA.elem (TDS.reflectSymbol (Proxy :: Proxy "reportReason")) erroredFields) }] "Please choose a reason",
                              HE.div (HA.class' "report-comment") [
                                    HE.label_ "Comment",
                                    HE.input [HA.type' "text", HA.maxlength 300, HA.class' "modal-input", HA.onInput setReportComment]
                              ],
                              HE.div (HA.class' "buttons") [
                                    HE.button [HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal] "Cancel",
                                    HE.button [HA.class' "green-button danger", HA.onClick <<< SpecialRequest $ ReportUser reportedID ] "Report"
                              ]
                        ]
                  ConfirmLogout ->
                        HE.div (HA.class' "confirmation" ) [
                              HE.span (HA.class' "bold") "Do you really want to log out?",
                              HE.div (HA.class' "buttons") [
                                    HE.button [HA.class' "cancel", HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal] "Cancel",
                                    HE.button [HA.class' "green-button danger", HA.onClick Logout] "Logout"
                              ]
                        ]
                  _ ->
                        HE.div (HA.class' "modal-placeholder") [
                              HE.div (HA.class' "modal-menu-mobile") [
                                    SIA.arrow [HA.class' "svg-back-card", HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal],
                                    HE.strong_ $ show toggle
                              ],
                              HE.div (HA.class' "modal-menu") [
                                    HE.div [HA.onClick <<< SpecialRequest $ ToggleModal HideUserMenuModal, HA.class' "back"] [
                                          HE.svg [HA.class' "svg-16", HA.viewBox "0 0 30 30"][
                                                HE.path' [HA.d "M30 13.125H7.18125L17.6625 2.64375L15 0L0 15L15 30L17.6437 27.3563L7.18125 16.875H30V13.125Z"]
                                          ],
                                          HE.text " Back to chats"
                                    ],
                                    HE.div [HA.onClick <<< SpecialRequest $ ToggleModal ShowProfile, HA.class' { entry: true, selected: toggle == ShowProfile }] $ show ShowProfile,
                                    HE.div [HA.onClick <<< SpecialRequest $ ToggleModal ShowSettings, HA.class' { entry: true, selected: toggle == ShowSettings }] $ show ShowSettings,
                                    HE.div [HA.onClick <<< SpecialRequest $ ToggleModal ShowBacker, HA.class' { entry: true, selected: toggle == ShowBacker }] $ show ShowBacker,
                                    HE.div [HA.onClick <<< SpecialRequest $ ToggleModal ShowExperiments, HA.class' { entry: true, selected: toggle == ShowExperiments }] $ show ShowExperiments,
                                    HE.div [HA.onClick <<< SpecialRequest $ ToggleModal ShowLeaderboard, HA.class' { entry: true, selected: toggle == ShowLeaderboard }] $ show ShowLeaderboard,
                                    HE.div [HA.onClick <<< SpecialRequest $ ToggleModal ShowHelp, HA.class' { entry: true, selected: toggle == ShowHelp }] $ show ShowHelp
                              ],
                              HE.div [HA.id "profile-edition-root", HA.class' { hidden: toggle /= ShowProfile }] $ retry ShowProfile,
                              HE.div [HA.id "settings-edition-root", HA.class' { hidden: toggle /= ShowSettings }] $ retry ShowSettings,
                              HE.div [HA.id "backer-root", HA.class' { hidden: toggle /= ShowBacker }] $ retry ShowBacker,
                              HE.div [HA.id "experiments-root", HA.class' { hidden: toggle /= ShowExperiments }] $ retry ShowExperiments,
                              HE.div [HA.id "karma-leaderboard-root", HA.class' { hidden: toggle /= ShowLeaderboard }] $ retry ShowLeaderboard,
                              HE.div [HA.id "help-root", HA.class' { hidden: toggle /= ShowHelp }] $ retry ShowHelp
                        ]
      ]
      where retry toggle = HE.div (HA.class' "retry-modal") [
                  SIVR.retry "Failed to load contents" (ToggleModal toggle) failedRequests,
                  HE.div' (HA.class' "loading")
            ]
            toRadio i reason = let idName = show i in
                  HE.div (HA.class' "reason") [
                        HE.input [HA.type' "radio", HA.id $ "report-" <> idName, HA.name "report-reason", HA.onInput (const (SetField (_ { reportReason = Just reason, erroredFields = [] })))],
                        HE.label (HA.for $ "report-" <> idName) $ show reason
                  ]

            setReportComment value = SetField (_ { reportComment = Just value })