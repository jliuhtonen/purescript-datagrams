module Datagram.UDP where

import Prelude

foreign import data Socket :: *
foreign import data SOCKET :: !

newtype SocketInfo = SocketInfo {
    port :: Int,
    address :: String,
    family :: String
}

instance showSocketInfo :: Show SocketInfo where
    show (SocketInfo { port: p, address: a, family: f }) = "port: " ++ show p ++ " address: " ++ show a ++ " family: " ++ show f

