module Datagram.UDP.Eff where

import Control.Monad.Eff
import Prelude hiding (bind)
import Data.Maybe
import Data.Function
import Datagram.UDP

foreign import createSocket :: forall eff. String -> Eff (socket :: SOCKET | eff) Socket
foreign import bind :: forall eff. Maybe Int -> Maybe String -> (SocketInfo -> Eff eff Unit) -> Socket -> Eff eff Unit
foreign import closeSocket :: forall eff. Socket -> Eff (socket :: SOCKET | eff) Unit
foreign import onMessage :: forall eff. (String -> RemoteAddressInfo -> Eff eff Unit) -> Socket -> Eff eff Unit
foreign import send :: forall eff. Socket -> String -> Int -> Int -> Int -> String -> Eff (socket :: SOCKET | eff) Unit
foreign import ref :: forall eff. Socket -> Eff (socket :: SOCKET | eff) Socket
foreign import unref :: forall eff. Socket -> Eff (socket :: SOCKET | eff) Socket
