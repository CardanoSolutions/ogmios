App.hs
Bridge.hs
Effects
Health
Health.hs
Json.hs
Metrics
Metrics.hs
Options.hs
Protocol
Server.hs
Trace.hs


Ogmios
  | App
    | Server
      | Http
      | WebSocket
    | Health                OK 
    | Metrics               OK
    | Version               OK

  | Pure
    | Json                  OK
    | Health                OK
    | Metrics               OK
    | Version               OK
    | Protocol              OK 
      | ChainSync           OK
      | TxSubmission        OK 
      | StateQuery          OK 

  | Effects
    | MonadClock
    | MonadLogger
    | MonadMetrics
    | MonadPipe
