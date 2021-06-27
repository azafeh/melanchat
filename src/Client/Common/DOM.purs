module Client.Common.DOM where

import Prelude

import Data.Function.Uncurried (Fn2, Fn1)
import Data.Function.Uncurried as DFU
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Effect (Effect)
import Effect.Exception as EE
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Shared.Path as SP
import Shared.Types (ContentType(..))
import Shared.IM.Types
import Shared.Unsafe as SU
import Web.DOM.Document as WDD
import Web.DOM.Element (Element)
import Web.DOM.Element as WDE
import Web.DOM.Element as WHE
import Web.DOM.Node as WDN
import Web.DOM.NonElementParentNode as WDNE
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode as WDP
import Web.Event.CustomEvent (CustomEvent)
import Web.Event.CustomEvent as WEC
import Web.Event.Event (EventType(..))
import Web.Event.Event as WEE
import Web.Event.EventTarget as WET
import Web.Event.Internal.Types (Event)
import Web.HTML as WH
import Web.HTML.HTMLDocument as WHHD
import Web.HTML.HTMLElement as WHHE
import Web.HTML.HTMLScriptElement as WHS
import Web.HTML.Window as HWH
import Web.HTML.Window as WHW
import Web.UIEvent.KeyboardEvent as WUK
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)

foreign import innerHTML_ :: EffectFn2 Element String Unit
foreign import innerText_ :: EffectFn1 Element String
foreign import pushState_ :: EffectFn1 String Unit

foreign import createCustomEvent_ :: forall value. Fn2 String value CustomEvent
foreign import customEventDetail_ :: forall value. Fn1 CustomEvent value

foreign import value_ :: EffectFn1 Element String
foreign import setValue_ :: EffectFn2 Element String Unit

foreign import toggleDisabled_ :: EffectFn1 Element Unit

foreign import documentHasFocus :: Effect Boolean

foreign import screenWidth :: Effect Int

foreign import requestNotificationPermission :: Effect Unit
foreign import notificationPermission :: Effect String

foreign import scrollIntoView_ :: EffectFn1 Element Unit

setChatExperiment :: EventType
setChatExperiment = EventType "setChatExperiment"

confirm :: String -> Effect Boolean
confirm message = do
      window <- WH.window
      HWH.confirm message window

-- | Adds an event to the given element.
addEventListener :: forall a . Element -> EventType -> (Event -> Effect a) -> Effect Unit
addEventListener element eventType handler = do
      listener <- WET.eventListener handler
      WET.addEventListener eventType listener false $ WDE.toEventTarget element

-- | Selects a single element.
unsafeGetElementByID :: ElementID -> Effect Element
unsafeGetElementByID elementID = do
      maybeElement <- getElementByID elementID
      DM.maybe (EE.throwException $ EE.error $ "No element with id:" <> show elementID) pure maybeElement

getElementByID :: ElementID -> Effect (Maybe Element)
getElementByID elementID = do
      window <- WH.window
      document <- WHHD.toDocument <$> WHW.document window
      WDNE.getElementById (show elementID) $ WDD.toNonElementParentNode document

-- | Selects a single element.
unsafeQuerySelector :: String -> Effect Element
unsafeQuerySelector selector = do
      maybeElement <- querySelector selector
      DM.maybe (EE.throwException $ EE.error $ "Selector returned no nodes:" <> selector) pure maybeElement

querySelector :: String -> Effect (Maybe Element)
querySelector selector = do
      window <- WH.window
      document <- WHHD.toDocument <$> WHW.document window
      WDP.querySelector (QuerySelector selector) $ WDD.toParentNode document

scrollDown :: Element -> Effect Unit
scrollDown element = do
      height <- WDE.scrollHeight element
      WDE.setScrollTop height element

scrollIntoView :: Element -> Effect Unit
scrollIntoView element = EU.runEffectFn1 scrollIntoView_ element

-- | Input value property or button inner text
value :: Element -> Effect String
value element = EU.runEffectFn1 value_ element

setValue :: Element -> String -> Effect Unit
setValue element value = EU.runEffectFn2 setValue_ element value

toggleDisabled :: Element -> Effect Unit
toggleDisabled = EU.runEffectFn1 toggleDisabled_

setInnerHTML :: Element -> String -> Effect Unit
setInnerHTML element = EU.runEffectFn2 innerHTML_ element

innerTextFromTarget :: Event -> Effect String
innerTextFromTarget event = EU.runEffectFn1 innerText_ $ SU.fromJust do
      target <- WEE.target event
      WDE.fromEventTarget target

tagNameFromTarget :: Event -> String
tagNameFromTarget event = WDE.tagName $ SU.fromJust do
      target <- WEE.target event
      WDE.fromEventTarget target

loadScript :: String -> Effect Unit
loadScript name = do
      window <- WH.window
      document <- WHW.document window
      script <- WDD.createElement "script" $ WHHD.toDocument document
      WHS.setSrc (SP.pathery JS name) <<< SU.fromJust $ WHS.fromElement script
      body <- SU.fromJust <$> WHHD.body document
      void <<< WDN.appendChild (WHE.toNode script) $ WHHE.toNode body

createElement :: String -> Effect Element
createElement tag = do
      window <- WH.window
      document <- WHW.document window
      WDD.createElement tag $ WHHD.toDocument document

onEnter :: Element -> Effect Unit -> Effect Unit
onEnter element action = do
      addEventListener element keyup go
      where go event = do
                  let pressed = WUK.key <<< SU.fromJust $ WUK.fromEvent event
                  when (pressed == "Enter") action

preventStop :: Event -> Effect Unit
preventStop event = do
      WEE.preventDefault event
      WEE.stopPropagation event

setTitle :: String -> Effect Unit
setTitle title = do
      window <- WH.window
      document <- WHW.document window
      WHHD.setTitle title document

pushState :: String -> Effect Unit
pushState = EU.runEffectFn1 pushState_