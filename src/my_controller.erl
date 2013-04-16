-module(my_controller).

-export([start/0,link/2,tap/2]).

start()->
    loom_controller:start().


link(Port1,Port2)->
    Mod1 = loom_flow_lib:gate_mod(Port1,Port2),
    loom_controller:broadcast_flow_mod(Mod1),
    Mod2 = loom_flow_lib:gate_mod(Port2,Port1),
    loom_controller:broadcast_flow_mod(Mod2).

tap(InPort,OutPorts)->
    Mod1 = loom_flow_lib:tap_mod(InPort,OutPorts),
    loom_controller:broadcast_flow_mod(Mod1).
