module Shared.Experiments.Impersonation where

import Prelude
import Shared.Types

import Data.Array as DA
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Experiments.Types
import Shared.Avatar as SA
import Shared.Options.File (imageBasePath)
import Shared.Unsafe as SU

joined :: ImpersonationProfile -> Html ChatExperimentMessage
joined profile = HE.div (HA.class' "exit-impersonation") [
      HE.strong_ "You have already joined the Impersonation Experiment",
      HE.button [HA.class' "green-button danger exit-experiment", HA.onClick QuitExperiment ] "Exit"
]

view :: ChatExperimentModel -> Html ChatExperimentMessage
view { section, impersonation } = HE.div (HA.class' "impersonation") [
      header Characters "Characters",
      profiles Characters [batman],
      header HistoricalFigures "Historical figures",
      profiles HistoricalFigures [socrates],
      header Celebrities "Celebrities",
      profiles Celebrities [nicolasCage],
      HE.div (HA.class' {"modal-placeholder-overlay": true, hidden: DM.isNothing impersonation })[
            HE.div (HA.class' "confirmation") [
                  HE.span (HA.class' "bold") $ "Start Impersonation Experiment as " <> DM.maybe "" _.name impersonation <> "?",
                  HE.div (HA.class' "buttons") [
                        HE.button [HA.class' "cancel", HA.onClick $ ConfirmImpersonation Nothing] "Cancel",
                        HE.button [HA.class' "green-button", HA.onClick <<< JoinExperiment $ Impersonation impersonation ] "Start"
                  ]
            ]
      ]
]
      where header s name = HE.div [HA.class' "impersonation-header", HA.onClick $ ToggleSection s] [
                  HE.text name,
                  HE.span (HA.class' "header-plus") $ if section == s then "-" else "+"
            ]

            profiles s = HE.div (HA.class' { hidden : section /= s}) <<< DA.mapWithIndex toProfile
            toProfile index p@{ avatar, name, headline } = HE.div [HA.class' "contact", HA.onClick <<< ConfirmImpersonation $ Just p] [
                  HE.div (HA.class' "avatar-contact-list-div") [
                        HE.img [HA.title $ SU.fromJust avatar, HA.class' $ "avatar-contact-list" <> SA.avatarColorClass (Just index), HA.src $ SU.fromJust avatar]
                  ],
                  HE.div [HA.class' "contact-profile", HA.title $ "Start Impersonation as " <> name] [
                        HE.span (HA.class' "contact-name") name,
                        HE.span (HA.class' "duller") headline
                  ]
            ]

batman :: ImpersonationProfile
batman = {
      id : 1,
      name: "Batman",
      avatar: Just $ imageBasePath <> "batman_noun_project_Anusha_Narvekar.png",
      headline: "*raspy voice* I am Batman",
      description: "I am not afraid of bats. Don't tell Robin I am here.",
      tags : ["Martial arts", "Detective work", "Costumes", "Bats"],
      karma: 877,
      karmaPosition: 342,
      gender: Just $ show Male,
      country: Just "Gotham",
      languages: [],
      age: Just 34
}

socrates :: ImpersonationProfile
socrates = {
      id : 2,
      name: "Socrates",
      avatar: Just $ imageBasePath <> "socrates_Sting_wikimedia.png",
      headline: "I know that I know nothing",
      description: """Crito, we owe a rooster to Asclepius. Please, don't forget to pay the debt.

Crito, we owe a rooster to Asclepius, Pay it and do not neglect it.

Crito, we owe a rooster to Asclepius, make this offering to him and do not forget.""",
      tags : ["Philosophy", "Poison", "Debating", "Youth"],
      karma: 879,
      karmaPosition: 345,
      gender: Just $ show Male,
      country: Just "Greece",
      languages: [],
      age: Just 71
}

nicolasCage :: ImpersonationProfile
nicolasCage = {
      id : 3,
      name: "Nicolas Cage",
      avatar: Just $ imageBasePath <> "nicolas_cage_hiclipart.png",
      headline: "I think I jump around more when I'm alone",
      description: """Oh, boy! Oh, boy! Three and a half million dollar budget, some 16mm film stock thrown in, and I'm holding one of these. I have got to thank the members of the Academy for this, for including me in this group of super talents and for helping me blur the line between art and commerce with this award. I know it's not hip to say it, but I just love acting, and I hope that there'll be more encouragement for alternative movies where we can experiment and fast forward into the future of acting.

Let me thank the awesome, multi-talented Mike Figgis. My incredible, amazing co-star Elisabeth Shue. I am going to share this award with both of you and the late John O'Brien, whose spirit moved me so much. Tony Dingman, Annie Stewart -- the producers, Annie Stewart and Stuart Regen, everyone at MGM/UA and Lumiere. I'd like to thank Ed Limato, my colleagues Gerry Harrington, Jeff Levine, Richard Lovett. Everyone in my family, my gorgeous wife Patricia. And I just finally want to say: Hi, Weston. It's Daddy. I love you. Thank you.""",
      tags : ["Movies", "Funny faces", "Acting", "Money"],
      karma: 878,
      karmaPosition: 343,
      gender: Just $ show Male,
      country: Just "United States",
      languages: [],
      age: Just 57
}

impersonations :: HashMap Int ImpersonationProfile
impersonations = DH.fromFoldable [Tuple batman.id batman, Tuple socrates.id socrates, Tuple nicolasCage.id nicolasCage]

welcomeMessage :: String -> _
welcomeMessage name = { welcome: "You are impersonating: " <> name, first: "Tip: quit the experiment at any time ", second: "to go back to your chats" }