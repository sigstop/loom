-module(my_controller).

-export([start/0,link/2,forward/2,link_and_tap/3,link_and_tap2/3, clear/0]).

start()->
    loom_controller:start().
 %   timer:sleep(2000),

clear()->
    loom_controller:clear_all_flow_mods().

forward(InPort,OutPorts)->
    Mod= loom_flow_lib:forward_mod(InPort,OutPorts),
    loom_controller:broadcast_flow_mod(Mod).


%%% User defined things

link(Port1,Port2)->
    forward(Port1,[Port2]),
    forward(Port2,[Port1]).


link_and_tap(Port1,Port2, TapPorts)->
    forward(Port1,[Port2 | TapPorts]),
    forward(Port2,[Port1 | TapPorts]).

link_and_tap2(Port1,Port2, TapPorts)->
    forward(Port1,[Port2]),
    forward(Port2,[Port1 | TapPorts]).

