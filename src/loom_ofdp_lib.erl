-module(loom_ofdp_lib).

-compile([export_all]).
-export([link/3,forward/3,link_and_tap/4,link_and_tap2/4,clear/1]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

clear(Pid)->
    loom_ofdp:send(Pid,remove_all_flows).

forward(Pid,InPort,OutPorts)->
    Msg = loom_flow_lib:forward_mod(InPort,OutPorts),
    loom_ofdp:send_ofp_msg(Pid,Msg).

link(Pid,Port1,Port2)->
    forward(Pid,Port1,[Port2]),
    forward(Pid,Port2,[Port1]).

link_and_tap(Pid, Port1,Port2, TapPorts)->
    forward(Pid, Port1,[Port2 | TapPorts]),
    forward(Pid, Port2,[Port1 | TapPorts]).

link_and_tap2(Pid, Port1,Port2, TapPorts)->
    forward(Pid, Port1,[Port2]),
    forward(Pid, Port2,[Port1 | TapPorts]).

get_flow_table(Pid,TableId)->
    Msg = loom_ofmsg_lib:flow_stats_request(TableId),
    loom_ofdp:send_ofp_msg(Pid,Msg),
    loom:get_msg().
    
