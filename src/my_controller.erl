-module(my_controller).

-export([start/0,link/2,tap/3,gate/2,clear/0]).

start()->
    loom_controller:start().
 %   timer:sleep(2000),

clear()->
    loom_controller:clear_all_flow_mods().



link(Port1,Port2)->
    Mod1 = loom_flow_lib:gate_mod(Port1,Port2,12),
    loom_controller:broadcast_flow_mod(Mod1),
    Mod2 = loom_flow_lib:gate_mod(Port2,Port1,12),
    loom_controller:broadcast_flow_mod(Mod2).

tap(InPort,OutPort,TapPort)->
    gate(InPort,OutPort),
    gate(OutPort,TapPort),
    gate(TapPort,InPort).
%    Mod1 = loom_flow_lib:tap_mod(InPort,OutPorts,Seq),
%    loom_controller:broadcast_flow_mod(Mod1).


gate(InPort,OutPort)->
    Mod1= loom_flow_lib:gate_mod(InPort,OutPort,14),
    loom_controller:broadcast_flow_mod(Mod1).
