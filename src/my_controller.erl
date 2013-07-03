-module(my_controller).

-compile([export_all]).
-export([start/0,start/1,dp_link/3,dp_forward/3,match_forward/4,drop_loops/2,drop_loops1/2,
         dp_link_and_tap/4,dp_link_and_tap2/4, dp_clear/1, send_of_requests/1]).

start()->
    loom_controller:start().
 %   timer:sleep(2000),

start(Version)->
    loom_controller:start_link(6633,Version).

dp_clear(Pid)->
    loom_ofdp:send(Pid,remove_all_flows).

dp_forward(Pid,InPort,OutPorts)->
    Msg = loom_flow_lib:forward_mod(InPort,OutPorts),
    loom_ofdp:send_ofp_msg(Pid,Msg).

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

dp_link(Pid,Port1,Port2)->
    dp_forward(Pid,Port1,[Port2]),
    dp_forward(Pid,Port2,[Port1]).



dp_link_and_tap(Pid, Port1,Port2, TapPorts)->
    dp_forward(Pid, Port1,[Port2 | TapPorts]),
    dp_forward(Pid, Port2,[Port1 | TapPorts]).

dp_link_and_tap2(Pid, Port1,Port2, TapPorts)->
    dp_forward(Pid, Port1,[Port2]),
    dp_forward(Pid, Port2,[Port1 | TapPorts]).



test_drop_loop()->
    drop_loops(6,<<10,192,168,86>>).

link_macs(TapPorts)->
    Mod1 = loom_flow_lib:match_forward_mod(5,<<16#b8,16#27,16#eb,16#bc,16#69,16#c8>>,[6|TapPorts]),
    loom_controller:broadcast_flow_mod(Mod1),
    Mod2 = loom_flow_lib:match_forward_mod(6,<<16#b8,16#27,16#eb,16#f0,16#cc,16#c0>>,[5|TapPorts]),
    loom_controller:broadcast_flow_mod(Mod2).


test_overlap()->             
    Mod1 = loom_flow_lib:match_forward_mod(5,
					   <<16#b8,16#27,16#eb,16#bc,16#69,16#c8>>,
					   [6],
					   250,
					   [check_overlap]),
    Mod2 = loom_flow_lib:forward_mod(5,[6],250,[check_overlap]),
    loom_controller:broadcast_flow_mod(Mod1),
    loom_controller:broadcast_flow_mod(Mod2).  %% Should fail

test_overlap2()->             
    Mod1 = loom_flow_lib:match_forward_mod(5,
					   <<16#b8,16#27,16#eb,16#bc,16#69,16#c8>>,
					   [6],
					   250,
					   [check_overlap]),
    Mod2 = loom_flow_lib:forward_mod(5,[6],250,[check_overlap]),
    loom_controller:broadcast_flow_mod(Mod2),
    loom_controller:broadcast_flow_mod(Mod1).  %% Should fail
    
    

test_drop_loop1()->
    dp_forward(fix,6,[5]),
    drop_loops1(5,<<16#b8,16#27,16#eb,16#bc,16#69,16#c8>>).
    

print_mod()->
    io:format("~p~n",[loom_flow_lib:forward_mod(5,[6,7])]).

print_mod2()->
    Mod = loom_flow_lib:drop_loops_mod1(6,<<16#b8,16#27,16#eb,16#bc,16#69,16#c8>>),
    io:format("~w~n",[Mod]).

get_flow_table(TableId)->
    Mod = loom_flow_lib:get_flow_table_message(TableId),
    loom_controller:broadcast_flow_mod(Mod).

config_packet_in(Param)->
    Mod = loom_flow_lib:config_packet_in(Param),
    loom_controller:broadcast_flow_mod(Mod).

role_request(Role)->
    GenId = 1,
    Request = loom_flow_lib:role_request(Role, GenId),
    loom_controller:broadcast_flow_mod(Request).

%% send requests to dp
send_of_requests(Pid)->
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:features_request()),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:echo_request()),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:get_config_request()),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:desc_request()),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:flow_stats_request(0)),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:aggregate_stats_request(0)),
% results are very long : 256 tables in LINC
%    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:table_stats_request()),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:port_stats_request(any)),
% Getting error reponse for queue_stats_request: "Received Message from #Port<0.7204>: {ofp_message,4,error,1,{ofp_error_msg,queue_op_failed,bad_queue,<<>>}} 
%    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:queue_stats_request(any)),       
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:group_stats_request()),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:group_desc_request()),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:group_features_request()),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:meter_stats_request(all)),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:meter_config_request(1)),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:meter_features_request()),
% results are very long : 256 tables in LINC
%    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:table_features_request()),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:port_desc_request()),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:queue_get_config_request(all)),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:role_request(nochange, 100)),
    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:get_async_config()).
