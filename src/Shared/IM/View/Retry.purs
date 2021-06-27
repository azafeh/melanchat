module Shared.IM.View.Retry where

import Prelude
import Shared.Types

import Data.Foldable as DA
import Flame (Html)
import Shared.IM.Types
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

retry :: String -> RetryableRequest -> Array RequestFailure -> Html IMMessage
retry failedText requestMessage failedRequests = HE.div [HA.class' { retry: true, invisible: not $ DA.any ((requestMessage == _) <<< _.request) failedRequests }] <<< retryForm failedText $ SpecialRequest requestMessage

retryForm :: String -> IMMessage -> Array (Html IMMessage)
retryForm failedText message = [
      HE.text failedText,
      HE.span [HA.class' "retry-button", HA.onClick message ] [
            HE.text "Retry",
            retryIcon "svg-16"
      ]
]

retryIcon :: String -> Html IMMessage
retryIcon class' =
      HE.svg [HA.class' class', HA.viewBox "0 0 16 16"] [
            HE.path' [HA.class' "strokeless", HA.d "M13.75,8l1.76-2.69-1.87.39,0,0a6.12,6.12,0,0,0-8-3.27,6.11,6.11,0,0,0,0,11.29,6.1,6.1,0,0,0,8-3.33A.37.37,0,1,0,13,10,5.4,5.4,0,0,1,10.05,13a5.37,5.37,0,1,1,2.83-7.12l-1.83.38Z"]
      ]