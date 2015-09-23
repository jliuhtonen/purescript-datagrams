module Datagram.UDP.Eff(
    createSocket,
    bind,
    closeSocket,
    onMessage,
    send,
    ref,
    unref    
) where

import Control.Monad.Eff
import Prelude hiding (bind)
import Data.Maybe
import Data.Function
import Node.Buffer
import Datagram.UDP

createSocket :: forall eff. SocketType -> Eff (socket :: SOCKET | eff) Socket
createSocket socketType = _createSocket $ show socketType

foreign import _createSocket :: forall eff. String -> Eff (socket :: SOCKET | eff) Socket
foreign import bind :: forall eff. Maybe Int -> Maybe String -> (SocketInfo -> Eff eff Unit) -> Socket -> Eff eff Unit
foreign import closeSocket :: forall eff. Socket -> Eff (socket :: SOCKET | eff) Unit
foreign import onMessage :: forall eff. (Buffer -> RemoteAddressInfo -> Eff eff Unit) -> Socket -> Eff eff Unit
foreign import send :: forall eff. Buffer -> Int -> Int -> Int -> String -> Socket -> Eff (socket :: SOCKET | eff) Unit
foreign import ref :: forall eff. Socket -> Eff (socket :: SOCKET | eff) Socket
foreign import unref :: forall eff. Socket -> Eff (socket :: SOCKET | eff) Socket
