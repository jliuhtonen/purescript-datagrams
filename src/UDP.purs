module Datagram.UDP where

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Node.Buffer
import Prelude hiding (bind)
import Data.Maybe

foreign import data Socket :: *
foreign import data SOCKET :: !

data SocketType = UDP4 | UDP6

newtype SocketInfo = SocketInfo {
    port :: Int,
    address :: String,
    family :: String
}

newtype RemoteAddressInfo = RemoteAddressInfo {
    address :: String,
    port :: Int
}

instance showSocketType :: Show SocketType where
    show UDP4 = "udp4"
    show UDP6 = "udp6"

instance showSocketInfo :: Show SocketInfo where
    show (SocketInfo { port: p, address: a, family: f }) = "port: " ++ show p ++ " address: " ++ show a ++ " family: " ++ show f

instance showRemoteAddressInfo :: Show RemoteAddressInfo where
    show (RemoteAddressInfo { address: a, port: p }) = "address: " ++ show a ++ " port: " ++ show p

createSocket :: forall eff. SocketType -> Aff (socket :: SOCKET | eff) Socket
createSocket socketType = liftEff <<< _createSocket $ show socketType

closeSocket :: forall eff. Socket -> Aff (socket :: SOCKET | eff) Unit
closeSocket = liftEff <<< _closeSocket

onMessage :: forall eff. (Buffer -> RemoteAddressInfo -> Eff eff Unit) -> Socket -> Aff (socket :: SOCKET | eff) Unit
onMessage msgHandler socket = liftEff $ _onMessage msgHandler socket

send :: forall eff. Buffer -> Int -> Int -> Int -> String -> Socket -> Aff (socket :: SOCKET | eff) Unit
send buffer offset length port address socket = liftEff $ _send buffer offset length port address socket

ref :: forall eff. Socket -> Aff (socket :: SOCKET | eff) Socket
ref = liftEff <<< _ref

unref :: forall eff. Socket -> Aff (socket :: SOCKET | eff) Socket
unref = liftEff <<< _unref

foreign import _createSocket :: forall eff. String -> Eff (socket :: SOCKET | eff) Socket
foreign import bind :: forall eff. Maybe Int -> Maybe String -> Socket -> Aff (socket :: SOCKET | eff) SocketInfo
foreign import _closeSocket :: forall eff. Socket -> Eff (socket :: SOCKET | eff) Unit
foreign import _onMessage :: forall eff. (Buffer -> RemoteAddressInfo -> Eff eff Unit) -> Socket -> Eff (socket :: SOCKET | eff) Unit
foreign import _send :: forall eff. Buffer -> Int -> Int -> Int -> String -> Socket -> Eff (socket :: SOCKET | eff) Unit
foreign import _ref :: forall eff. Socket -> Eff (socket :: SOCKET | eff) Socket
foreign import _unref :: forall eff. Socket -> Eff (socket :: SOCKET | eff) Socket

