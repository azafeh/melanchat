module Client.Common.Location where

import Prelude

import Data.Function.Uncurried (Fn2)
import Data.Function.Uncurried as DF
import Data.Maybe (Maybe(..))
import Data.String as DS
import Effect (Effect)
import Shared.IM.Types (ElementID)
import Web.HTML as WH
import Web.HTML.Location as WHL
import Web.HTML.Window as WHW

foreign import getParameter_ :: Fn2 String String String

setLocation :: String -> Effect Unit
setLocation route = do
      window <- WH.window
      location <- WHW.location window
      WHL.setHref route location

search :: Effect String
search = do
      window <- WH.window
      location <- WHW.location window
      WHL.search location

path :: Effect String
path = do
      window <- WH.window
      location <- WHW.location window
      search' <-WHL.search location
      pathname <- WHL.pathname location
      pure $ pathname <> search'

queryParameter :: String -> Effect (Maybe String)
queryParameter name = do
      search' <- search
      let parameter = DF.runFn2 getParameter_ search' name
      pure $ if DS.null parameter then Nothing else Just parameter

hash :: Effect String
hash = do
      window <- WH.window
      location <- WHW.location window
      WHL.hash location

setHash :: ElementID -> Effect Unit
setHash elementID = do
      window <- WH.window
      location <- WHW.location window
      WHL.setHash (show elementID) location

hostName :: Effect String
hostName = do
      window <- WH.window
      location <- WHW.location window
      WHL.hostname location

reload :: Effect Unit
reload = do
      window <- WH.window
      location <- WHW.location window
      WHL.reload location