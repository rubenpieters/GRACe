import Compile0
import GL

{-%
   "name"       : "rain",
 , "parameters" : [ {"name" : "amount",   "type" : "Float"} ]
 , "interface"  : [ {"name" : "rainfall", "type" : "Flow" } ]
 %-}
rain :: Float -> GCM (Port Float)
rain amount = do
  port <- createPort
  set port amount
  return port

{-%
   "name"       : "pump"
 , "parameters" : [ {"name" : "capacity", "type" : "Float"} ]
 , "interface"  : [ {"name" : "inflow",   "type" : "Flow" }
                  , {"name" : "outflow",  "type" : "Flow" }
                  ]
 %-}
pump :: Float -> GCM (Port Float, Port Float)
pump maxCap = do
  inPort  <- createPort
  outPort <- createPort

  component $ do
    inflow <- value inPort
    outflow <- value outPort

    assert $ inflow === outflow
    assert $ inflow `inRange` (0, lit maxCap)

  return (inPort, outPort)

{-%
   "name"       : "runoff area"
 , "parameters" : [ {"name" : "storage capacity", "type" : "Float"} ]
 , "interface"  : [ {"name" : "inflow",           "type" : "Flow" }
                  , {"name" : "outlet",           "type" : "Flow" }
                  , {"name" : "overflow",         "type" : "Flow" }
                  ]
 %-}
runoffArea :: Float -> GCM (Port Float, Port Float, Port Float)
runoffArea cap = do
  inflow <- createPort
  outlet <- createPort
  overflow <- createPort

  component $ do
    currentStored <- createLVar

    inf <- value inflow
    out <- value outlet
    ovf <- value overflow
    sto <- value currentStored

    assert $ sto === inf - out - ovf
    assert $ sto `inRange` (0, lit cap)
    assert $ (ovf .> 0) ==> (sto === lit cap)
    assert $ ovf .>= 0

  return (inflow, outlet, overflow)

example :: GCM ()
example = do
  (inflowP, outflowP) <- pump 5
  (inflowS, outletS, overflowS) <- runoffArea 5
  rainflow <- rain 10

  link inflowP outletS
  link inflowS rainflow

  output overflowS "Overflow"