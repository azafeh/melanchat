module Client.IM.WebSocket (sendPayload, module WSW, module WSEM, module WSEE, createWebSocket) where

import Prelude
import Shared.Types

import Client.Common.Location as CCD
import Data.Boolean (otherwise)
import Effect (Effect)
import Environment (development)
import Shared.JSON as SJ
import Shared.Options.WebSocket (port)
import Web.Socket.Event.EventTypes as WSEE
import Shared.IM.Types
import Web.Socket.Event.MessageEvent as WSEM
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WSWS
import Web.Socket.WebSocket hiding (sendString,create) as WSW

createWebSocket :: Effect WebSocket
createWebSocket = do
      hostName <- CCD.hostName
      WSWS.create (protocol <> hostName <> endpoint) []
      where protocol
                  | development = "ws://"
                  | otherwise = "wss://"
            endpoint
                  | development = ":" <> show port
                  | otherwise = "/ws"

sendPayload :: WebSocket -> WebSocketPayloadServer -> Effect Unit
sendPayload ws = WSWS.sendString ws <<< SJ.toJSON