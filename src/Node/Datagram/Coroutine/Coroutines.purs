module Node.Datagram.Coroutine(
  SocketClosed(),
  DatagramEvent(..),
  datagramEventProducer
  ) where

import Control.Coroutine
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Coroutine.Aff
import Data.Either
import Node.Datagram
import Node.Buffer
import Prelude

type SocketClosed = Unit
data DatagramEvent = DatagramValue Buffer RemoteAddressInfo | DatagramError Error

datagramEventProducer :: forall eff. Socket ->
  Producer DatagramEvent (Aff (avar :: AVAR, socket :: SOCKET | eff)) SocketClosed
datagramEventProducer socket = produce eventEmitter where
  eventEmitter emit = do
    let msgHandler = \buffer rinfo -> emit (Left (DatagramValue buffer rinfo))
    let errorHandler = \err -> emit (Left (DatagramError err))
    let closeHandler = \_ -> emit (Right unit)
    onMessage' msgHandler socket
    onError' errorHandler socket
    onClose' closeHandler socket
    pure unit
