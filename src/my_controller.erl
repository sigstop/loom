-module(my_controller).

-export([start/0,bridge/2]).

start()->
    loom_controller:start().


bridge(Port1,Port2)->
    Mod1 = loom_flow_lib:gate_mod(Port1,Port2),
    loom_controller:broadcast_flow_mod(Mod1),
    Mod2 = loom_flow_lib:gate_mod(Port2,Port1),
    loom_controller:broadcast_flow_mod(Mod2).
