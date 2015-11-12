## Module Node.Datagram.Coroutine

#### `SocketClosed`

``` purescript
type SocketClosed = Unit
```

#### `DatagramEvent`

``` purescript
data DatagramEvent
  = DatagramValue Buffer RemoteAddressInfo
  | DatagramError Error
```

#### `datagramEventProducer`

``` purescript
datagramEventProducer :: forall eff. Socket -> Producer DatagramEvent (Aff (avar :: AVAR, socket :: SOCKET | eff)) SocketClosed
```


