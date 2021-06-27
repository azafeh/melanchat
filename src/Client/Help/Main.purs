module Client.Help.Main where

import Prelude

import Client.Common.DOM as CCD
import Client.Common.Location as CCL
import Data.Traversable as DT
import Effect (Effect)

import Web.DOM.Element as WDE
import Web.Event.EventTarget as WET
import Web.HTML as WH
import Web.HTML.Event.EventTypes (click)
import Web.HTML.Event.HashChangeEvent.EventTypes (hashchange)
import Web.HTML.Window as WHW
import Shared.IM.Types

-- :(
main :: Effect Unit
main = do
      hash <- CCL.hash
      faqLink <- CCD.unsafeGetElementByID FaqLink
      termsLink <- CCD.unsafeGetElementByID TermsLink
      privacyLink <- CCD.unsafeGetElementByID PrivacyLink
      faq <- CCD.unsafeGetElementByID Faq
      terms <- CCD.unsafeGetElementByID TermsSection
      privacy <- CCD.unsafeGetElementByID PrivacySection

      let   unselectAll = do
                  DT.traverse_ (WDE.setAttribute "class" "entry") [faqLink, termsLink, privacyLink]
                  DT.traverse_ (WDE.setAttribute "class" "hidden") [faq, terms, privacy]
            select link tab = do
                  unselectAll
                  WDE.setAttribute "class" "entry selected" link
                  WDE.setAttribute "class" "" tab
            showTab =
                  case _ of
                        "#terms" -> select termsLink terms
                        "#privacy" -> select privacyLink privacy
                        _ -> select faqLink faq

      CCD.addEventListener faqLink click (const (CCL.setHash Faq))
      CCD.addEventListener termsLink click (const (CCL.setHash TermsSection))
      CCD.addEventListener privacyLink click (const (CCL.setHash PrivacySection))

      hashListener <- WET.eventListener $ const (CCL.hash >>= showTab)
      window <- WH.window
      WET.addEventListener hashchange hashListener false $ WHW.toEventTarget window

      showTab hash
