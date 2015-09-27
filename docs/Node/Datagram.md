## Module Node.Datagram

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
bindSocket :: forall eff. Maybe Int -> Maybe String -> Socket -> Aff (socket :: SOCKET | eff) SocketInfo
```

#### `onMessage`

``` purescript
onMessage :: forall eff. (Buffer -> RemoteAddressInfo -> Eff eff Unit) -> Socket -> Aff (socket :: SOCKET | eff) Unit
```

#### `onError`

``` purescript
onError :: forall eff. (Error -> Eff eff Unit) -> Socket -> Aff (socket :: SOCKET | eff) Unit
```

#### `send`

``` purescript
send :: forall eff. Buffer -> Int -> Int -> Int -> String -> Socket -> Aff (socket :: SOCKET | eff) Unit
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
addMembership :: forall eff. String -> Maybe String -> Socket -> Aff (socket :: SOCKET | eff) Socket
```

#### `dropMembership`

``` purescript
dropMembership :: forall eff. String -> Maybe String -> Socket -> Aff (socket :: SOCKET | eff) Socket
```

#### `address`

``` purescript
address :: forall eff. Socket -> Aff (socket :: SOCKET | eff) SocketInfo
```


