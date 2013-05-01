-module(my_controller).

-compile([export_all]).
-export([start/0,start/1,link/2,forward/2,match_forward/4,drop_loops/2,drop_loops1/2,link_and_tap/3,link_and_tap2/3, clear/0]).

start()->
    loom_controller:start().
 %   timer:sleep(2000),

start(Version)->
    loom_controller:start_link(6633,Version).

clear()->
    loom_controller:remove_all_flows().

forward(InPort,OutPorts)->
    Mod = loom_flow_lib:forward_mod(InPort,OutPorts),
    loom_controller:broadcast_flow_mod(Mod).

match_forward(InPort,EthDst,IPv4Dst,OutPorts)->
    Mod = loom_flow_lib:matchforward_mod(InPort,EthDst,IPv4Dst,OutPorts),
    loom_controller:broadcast_flow_mod(Mod).

drop_loops(InPort,IPv4Dst)->
    Mod = loom_flow_lib:drop_loops_mod(InPort,IPv4Dst),
    loom_controller:broadcast_flow_mod(Mod).

drop_loops1(InPort,EthDst)->
    Mod = loom_flow_lib:drop_loops_mod1(InPort,EthDst),
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


%%%% Testing

test_start()->
    start(),
    timer:sleep(3000),
    clear().


test_drop_loop()->
    drop_loops(6,<<10,192,168,86>>).

link_macs()->
    Mod1 = loom_flow_lib:match_forward_mod(5,<<16#b8,16#27,16#eb,16#bc,16#69,16#c8>>,6),
    loom_controller:broadcast_flow_mod(Mod1),
    Mod2 = loom_flow_lib:match_forward_mod(6,<<16#b8,16#27,16#eb,16#f0,16#cc,16#c0>>,5),
    loom_controller:broadcast_flow_mod(Mod2).
    

test_drop_loop1()->
    forward(6,[5]),
    drop_loops1(5,<<16#b8,16#27,16#eb,16#bc,16#69,16#c8>>).
    

print_mod()->
    io:format("~p~n",[loom_flow_lib:forward_mod(5,[6,7])]).

print_mod2()->
    Mod = loom_flow_lib:drop_loops_mod1(6,<<16#b8,16#27,16#eb,16#bc,16#69,16#c8>>),
    io:format("~w~n",[Mod]).
