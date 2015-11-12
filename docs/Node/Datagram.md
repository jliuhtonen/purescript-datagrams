## Module Node.Datagram

This module wraps Node's `dgram` module with FFI for usage from Purescript.
See `dgram` module [documentation](https://nodejs.org/api/dgram.html) for full details.

#### `Socket`

``` purescript
data Socket :: *
```

#### `SOCKET`

``` purescript
data SOCKET :: !
```

#### `SocketType`

``` purescript
data SocketType
  = UDP4
  | UDP6
```

##### Instances
``` purescript
instance showSocketType :: Show SocketType
```

#### `Port`

``` purescript
type Port = Int
```

#### `Address`

``` purescript
type Address = String
```

#### `Family`

``` purescript
type Family = String
```

#### `BufferLength`

``` purescript
type BufferLength = Int
```

#### `Interface`

``` purescript
type Interface = String
```

#### `MessageListener`

``` purescript
type MessageListener eff a = Buffer -> RemoteAddressInfo -> Eff eff a
```

#### `ErrorListener`

``` purescript
type ErrorListener eff a = Error -> Eff eff a
```

#### `SocketInfo`

``` purescript
newtype SocketInfo
```

##### Instances
``` purescript
instance showSocketInfo :: Show SocketInfo
```

#### `RemoteAddressInfo`

``` purescript
newtype RemoteAddressInfo
  = RemoteAddressInfo { address :: Address, port :: Port }
```

##### Instances
``` purescript
instance showRemoteAddressInfo :: Show RemoteAddressInfo
```

#### `createSocket`

``` purescript
createSocket :: forall eff. SocketType -> Aff (socket :: SOCKET | eff) Socket
```

#### `closeSocket`

``` purescript
closeSocket :: forall eff. Socket -> Aff (socket :: SOCKET | eff) Unit
```

#### `bindSocket`

``` purescript
bindSocket :: forall eff. Maybe Port -> Maybe Address -> Socket -> Aff (socket :: SOCKET | eff) SocketInfo
```

Binds an UDP socket for receiving messages.

Calling this function with port set to `Nothing` will bind the socket on a random port provided by the OS.
Calling this function with address set to `Nothing` will listen on all interfaces (`0.0.0.0`).

#### `onMessage`

``` purescript
onMessage :: forall a eff1 eff2. MessageListener eff1 a -> Socket -> Aff (socket :: SOCKET | eff2) Unit
```

Registers a listener for incoming messages on a bound socket.
Corresponds to Node's `socket.on('message', function(m) { ... })` functionality

#### `onMessage'`

``` purescript
onMessage' :: forall a eff1 eff2. MessageListener eff1 a -> Socket -> Eff (socket :: SOCKET | eff2) Unit
```

#### `onError`

``` purescript
onError :: forall a eff1 eff2. ErrorListener eff1 a -> Socket -> Aff (socket :: SOCKET | eff2) Unit
```

Register a listener for errors.
Corresponds to Node's `socket.on('error', function(e) { ... })` functionality

#### `onError'`

``` purescript
onError' :: forall a eff1 eff2. ErrorListener eff1 a -> Socket -> Eff (socket :: SOCKET | eff2) Unit
```

#### `onClose`

``` purescript
onClose :: forall eff1 eff2. (Unit -> Eff eff1 Unit) -> Socket -> Aff (socket :: SOCKET | eff2) Unit
```

#### `onClose'`

``` purescript
onClose' :: forall eff1 eff2. (Unit -> Eff eff1 Unit) -> Socket -> Eff (socket :: SOCKET | eff2) Unit
```

#### `onListening`

``` purescript
onListening :: forall eff1 eff2. (Unit -> Eff eff1 Unit) -> Socket -> Aff (socket :: SOCKET | eff2) Unit
```

#### `send`

``` purescript
send :: forall eff. Buffer -> Offset -> BufferLength -> Port -> Address -> Socket -> Aff (socket :: SOCKET | eff) Unit
```

#### `ref`

``` purescript
ref :: forall eff. Socket -> Aff (socket :: SOCKET | eff) Socket
```

#### `unref`

``` purescript
unref :: forall eff. Socket -> Aff (socket :: SOCKET | eff) Socket
```

#### `setBroadcast`

``` purescript
setBroadcast :: forall eff. Boolean -> Socket -> Aff (socket :: SOCKET | eff) Socket
```

#### `setTTL`

``` purescript
setTTL :: forall eff. Int -> Socket -> Aff (socket :: SOCKET | eff) Socket
```

#### `setMulticastTTL`

``` purescript
setMulticastTTL :: forall eff. Int -> Socket -> Aff (socket :: SOCKET | eff) Socket
```

#### `addMembership`

``` purescript
addMembership :: forall eff. Address -> Maybe Interface -> Socket -> Aff (socket :: SOCKET | eff) Socket
```

#### `dropMembership`

``` purescript
dropMembership :: forall eff. Address -> Maybe Interface -> Socket -> Aff (socket :: SOCKET | eff) Socket
```

#### `address`

``` purescript
address :: forall eff. Socket -> Aff (socket :: SOCKET | eff) SocketInfo
```


