module Shared.IM.View.Profile where

import Prelude
import Shared.Types

import Data.Array ((!!), (..), (:))
import Data.Array as DA
import Data.HashMap as HS
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Shared.Experiments.Types
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.IM.Types

import Shared.Experiments.Impersonation (impersonations)
import Shared.Experiments.Impersonation as SEI
import Shared.IM.Svg as SIA
import Shared.IM.View.Chat as SIVC
import Shared.IM.View.Retry as SIVR
import Shared.Markdown as SM
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU

--refactor: break this shit down into right modules

profile :: IMModel -> Html IMMessage
profile model@{ suggestions, contacts, suggesting, chatting, fullContactProfileVisible } =
      if DA.null suggestions && DM.isNothing chatting then
            emptySuggestions
       else
            case chatting, suggesting of
                  i@(Just index), _ ->
                        let cnt@{ user: { name }, available, impersonating } = contacts !@ index
                        in
                              if not available then
                                    unavailable name
                               else if fullContactProfileVisible then
                                    fullProfile FullContactProfile i model impersonating cnt.user
                               else
                                    contact model cnt
                  Nothing, (Just index) -> suggestion model index
                  _, _ -> emptySuggestions
      --this will need improvement
      where emptySuggestions = HE.div (HA.class' {"suggestion empty retry": true, hidden: DM.isJust chatting }) $ SIVR.retryForm "Could not find new suggestions" $ SpecialRequest NextSuggestion

unavailable :: String -> Html IMMessage
unavailable name =
      HE.div [HA.class' "profile-contact"] [
            HE.div (HA.class' "profile-contact-top") [
                  HE.div (HA.class' "profile-unavailable-header") [
                        SIA.arrow [HA.class' "svg-back-card", HA.onClick $ ToggleInitialScreen true],
                        HE.h1 (HA.class' "contact-name") name,
                        HE.span (HA.class' "unavailable-message") " is no longer available"
                  ]
            ]
]

contact :: IMModel -> Contact -> Html IMMessage
contact model@{ chatting, toggleContextMenu } cnt@{ impersonating, user: { id} } =
      HE.div (HA.class' "profile-contact") [
            HE.div (HA.class' "profile-contact-top") [
                  SIA.arrow [HA.class' "svg-back-card", HA.onClick $ ToggleInitialScreen true],
                  HE.img $ [HA.class' $ "avatar-profile " <> SA.avatarColorClass chatting, HA.src $ SA.avatarForRecipient chatting avatar] <> showProfile,
                  HE.div (HA.class' "profile-contact-header" : showProfile) [
                        HE.h1 (HA.class' "contact-name") name
                  ],
                  HE.div [HA.class' "profile-contact-deets"] <<<
                        HE.div [HA.class' "outer-user-menu"] $
                              SIA.contextMenu $ show CompactProfileContextMenu,
                              HE.div [HA.class' {"user-menu": true, visible: toggleContextMenu == ShowCompactProfileContextMenu }] $ blockReport id
            ],
            HE.div (HA.class' "show-profile-icon-div" : showProfile) $
                  HE.svg [HA.class' "show-profile-icon", HA.viewBox "0 0 16 16"] [
                        HE.rect' [HA.x "0.01", HA.y "2", HA.width "16", HA.height "2"],
                        HE.polygon' [HA.class' "strokeless", HA.points "8.01 16 16.01 6 0.01 6 8.01 16"]
                  ]
      ]
      where showProfile = [HA.title "Click to see full profile", HA.onClick ToggleContactProfile]
            { name, avatar } = case impersonating of
                  Just impersonationID -> SU.fromJust $ HS.lookup impersonationID impersonations
                  _ -> cnt.user

suggestion :: IMModel -> Int -> Html IMMessage
suggestion model@{ user, suggestions, experimenting } index =
      HE.div (HA.class' "suggestion-cards") [
            case experimenting of
                  Just (Impersonation (Just { name })) ->
                        let { welcome, first, second } = SEI.welcomeMessage name
                        in HE.div (HA.class' "card-top-header imp") [
                              HE.div (HA.class' "welcome") $ welcome,
                              HE.div (HA.class' "welcome-new") $ first <> second
                        ]
                  _ ->
                        HE.div (HA.class' "card-top-header") [
                              HE.div (HA.class' "welcome") $ "Welcome, " <> user.name,
                              HE.div (HA.class' "welcome-new") "Here are your newest chat suggestions"
                        ],
            HE.div (HA.class' "cards") cards
      ]
      where cards =
                  let available = DA.catMaybes <<< map (\i -> map (card model index i) $ suggestions !! i) $ (index - 1) .. (index + 1)
                  in case DA.length available of
                        1 -> dummyCard model : DA.snoc available (dummyCard model)
                        2 | index == 0 -> dummyCard model : available
                        2 | index > 0 -> DA.snoc available (dummyCard model)
                        _ -> available

card :: IMModel -> Int -> Int -> Suggestion -> Html IMMessage
card model suggesting index suggestion =
      let   isCenter = index == suggesting
            attrs
                  | isCenter = [ HA.class' "card card-center"  ]
                  | otherwise = [HA.class' "card card-sides faded" ]
      in HE.div attrs $ fullProfile (if isCenter then CurrentSuggestion else OtherSuggestion) (Just index) model Nothing suggestion

dummyCard :: IMModel -> Html IMMessage
dummyCard model = card model (-1) 0 dummySuggestion

dummySuggestion :: Suggestion
dummySuggestion = {
      id: 0,
      name: "Maria Navarro",
      headline: "This is my headline, there are many like it, but this one is mine",
      description: "Many years later, as he faced the firing squad, Colonel Aureliano Buendía was to remember that distant afternoon when his father took him to discover ice. At that time Macondo was a village of twenty adobe houses, built on the bank of a river of clear water that ran along a bed of polished stones, which were white and enormous, like prehistoric eggs. The world was so recent that many things lacked names, and in order to indicate them it was necessary to point. Every year during the month of March a family of ragged gypsies would set up their tents near the village, and with a great uproar of pipes and kettledrums they would display new inventions. First they brought the magnet.",
      avatar: Nothing,
      tags: [],
      karma: 321,
      karmaPosition : 90,
      gender: Just $ show Female,
      country: Just "Cali",
      languages: ["English", "Spanish"],
      age: Just 18
}

arrow :: Boolean -> IMMessage -> Html IMMessage
arrow freeToFetchSuggestions message = HE.div (HA.class' ("suggestion-arrow" <> e) : clickMessage) [
      case message of
            SpecialRequest PreviousSuggestion -> backArrow
            _ -> nextArrow
]
      where clickMessage
                  | freeToFetchSuggestions = [HA.onClick message]
                  | otherwise = []
            e = case message of
                   SpecialRequest PreviousSuggestion -> " ppe"
                   _ ->  ""

backArrow :: Html IMMessage
backArrow = HE.svg [HA.class' "svg-55", HA.viewBox "0 0 16 16"] [
      HE.circle' [HA.class' "strokeless", HA.cx "8", HA.cy "8", HA.r "8", HA.fill "#1B2921"],
      HE.polygon' [HA.class' "fillless strokeless", HA.points "4.88 7.99 9.37 3.5 10.29 4.42 6.73 7.99 10.32 11.58 9.39 12.5 5.81 8.91 5.8 8.91 4.88 7.99"]
]

nextArrow :: Html IMMessage
nextArrow = HE.svg [HA.class' "svg-55", HA.viewBox "0 0 16 16"] [
      HE.circle' [HA.class' "strokeless", HA.cx "8", HA.cy "8", HA.r "8", HA.fill "#1B2921"],
      HE.polygon' [HA.class' "fillless strokeless", HA.points "11.02 7.99 6.53 3.5 5.61 4.42 9.17 7.99 5.58 11.58 6.5 12.5 10.09 8.91 10.1 8.91 11.02 7.99"]
]

fullProfile :: ProfilePresentation -> Maybe Int -> IMModel -> Maybe Int -> IMUser -> Html IMMessage
fullProfile presentation index model@{ toggleContextMenu, freeToFetchSuggestions } impersonating user@{ id } =
      case presentation of
            FullContactProfile -> HE.div [HA.class' "suggestion old"] $ fullProfileMenu : profile
            CurrentSuggestion -> HE.div [HA.class' "suggestion-center"] [
                  HE.div [HA.class' "suggestion new"] $ loading : currentSuggestionMenu : profile,
                  HE.div [HA.class' "suggestion-input"] $ SIVC.chatBarInput ChatInputSuggestion model
            ]
            OtherSuggestion -> HE.div [HA.class' "suggestion new"] profile
      where profile = displayUserProfile index (case impersonating of
                  Just impersonationID ->
                        SU.fromJust $ HS.lookup impersonationID impersonations
                  _ ->
                        user
                  ) <> [
                  arrow freeToFetchSuggestions $ SpecialRequest PreviousSuggestion,
                  arrow freeToFetchSuggestions $ SpecialRequest NextSuggestion
            ]

            fullProfileMenu = HE.div (HA.class' "profile-top-menu") [
                  SIA.arrow [HA.class' "svg-back-profile", HA.onClick ToggleContactProfile],
                  HE.div [HA.class' "outer-user-menu"] $
                        SIA.contextMenu $ show FullProfileContextMenu,
                        HE.div [HA.class' {"user-menu": true, visible: toggleContextMenu == ShowFullProfileContextMenu }] $ blockReport id
            ]

            currentSuggestionMenu = HE.div [HA.class' "profile-context outer-user-menu"] [
                  SIA.arrow [HA.class' "svg-back-card", HA.onClick $ ToggleInitialScreen true],
                  SIA.contextMenu $ show SuggestionContextMenu,
                  HE.div [HA.class' {"user-menu": true, visible: toggleContextMenu == ShowSuggestionContextMenu }] $ blockReport id
            ]

            loading = HE.div' $ HA.class' { loading: true, hidden: freeToFetchSuggestions }

blockReport :: Int -> Array (Html IMMessage)
blockReport id = [
      HE.div [HA.class' "user-menu-item menu-item-heading", HA.onClick <<< SpecialRequest $ BlockUser id] "Block",
      HE.div [HA.class' "user-menu-item menu-item-heading", HA.onClick <<< SpecialRequest <<< ToggleModal $ ShowReport id] "Report"
]

displayUserProfile :: forall message. Maybe Int -> IMUser -> Array (Html message)
displayUserProfile index { id, karmaPosition, name, avatar, age, karma, headline, gender, country, languages, tags, description } = [
      HE.img [HA.class' $ "avatar-profile " <> SA.avatarColorClass index, HA.src $ SA.avatarForRecipient index avatar],
      HE.h1 (HA.class' "profile-name") name,
      HE.div (HA.class' "headline") headline,
      HE.div (HA.class' "profile-karma") [
            HE.div_ [
                  HE.span [HA.class' "span-info"] $ show karma,
                  HE.span [HA.class' "duller"] " karma",
                  HE.span_ $ " (#" <> show karmaPosition <> ")"
            ]
      ],
      HE.div (HA.class' "profile-asl") [
            HE.div_ [
                  toSpan $ map show age,
                  duller (DM.isNothing age || DM.isNothing gender) ", ",
                  toSpan gender
            ],
            HE.div_ [
                  duller (DM.isNothing country) "from ",
                  toSpan country
            ],
            HE.div_ ([
                  duller (DA.null languages) "speaks "
            ] <> (DA.intercalate [duller false ", "] $ map (DA.singleton <<< spanWith) languages))
      ],

      HE.div (HA.class' "tags-description") [
            HE.div (HA.class' "profile-tags") $ map toTagSpan tags,
            HE.span (HA.class' "duller profile-description-about" ) "About",
            HE.div' [HA.class' "description-message", HA.innerHtml $ SM.parse description]
      ]
]

toSpan :: forall message. Maybe String -> Html message
toSpan =
      case _ of
            Just s -> spanWith s
            _ -> HE.createEmptyElement "span"

spanWith :: forall message. String -> Html message
spanWith = HE.span (HA.class' "span-info")

toTagSpan :: forall message. String -> Html message
toTagSpan = HE.span (HA.class' "tag")

duller :: forall message. Boolean -> String -> Html message
duller hidden t = HE.span (HA.class' { "duller" : true, "hidden": hidden }) t