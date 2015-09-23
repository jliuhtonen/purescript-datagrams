module Datagram.UDP.Aff where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Node.Buffer
import Prelude hiding (bind)
import Data.Maybe

import Datagram.UDP
import qualified Datagram.UDP.Eff as U

createSocket :: forall eff. SocketType -> Aff (socket :: SOCKET | eff) Socket
createSocket = liftEff <<< U.createSocket

closeSocket :: forall eff. Socket -> Aff (socket :: SOCKET | eff) Unit
closeSocket = liftEff <<< U.closeSocket

onMessage :: forall eff. (Buffer -> RemoteAddressInfo -> Eff eff Unit) -> Socket -> Aff eff Unit
onMessage msgHandler socket = liftEff $ U.onMessage msgHandler socket

send :: forall eff. Buffer -> Int -> Int -> Int -> String -> Socket -> Aff (socket :: SOCKET | eff) Unit
send buffer offset length port address socket = liftEff $ U.send buffer offset length port address socket

ref :: forall eff. Socket -> Aff (socket :: SOCKET | eff) Socket
ref = liftEff <<< U.ref

unref :: forall eff. Socket -> Aff (socket :: SOCKET | eff) Socket
unref = liftEff <<< U.unref

bind :: forall eff. Maybe Int -> Maybe String -> Socket -> Aff eff SocketInfo
bind port address socket = makeAff $ \_ success -> U.bind port address success socket
