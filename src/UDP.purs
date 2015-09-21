module Datagram.UDP where

import Prelude

foreign import data Socket :: *
foreign import data SOCKET :: !

newtype SocketInfo = SocketInfo {
    port :: Int,
    address :: String,
    family :: String
}

newtype RemoteAddressInfo = RemoteAddressInfo {
    address :: String,
    port :: Int
}

instance showSocketInfo :: Show SocketInfo where
    show (SocketInfo { port: p, address: a, family: f }) = "port: " ++ show p ++ " address: " ++ show a ++ " family: " ++ show f

instance showRemoteAddressInfo :: Show RemoteAddressInfo where
    show (RemoteAddressInfo { address: a, port: p }) = "address: " ++ show a ++ " port: " ++ show p
