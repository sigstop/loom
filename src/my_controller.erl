%%------------------------------------------------------------------------------
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%-----------------------------------------------------------------------------
%%
%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox Inc
%% @doc 

-module(my_controller).

-compile([export_all]).
-export([start/0,start/1,dp_link/3,dp_forward/3,match_forward/4,drop_loops/2,drop_loops1/2,
         dp_link_and_tap/4,dp_link_and_tap2/4, dp_clear/1, send_of_requests/1,
         test_tap/0, test_tap2/0, tap_all/0, test_link12/0, test_link/3,
         test_clear/0, test_echo/1, send_flow_stats_req/1, show_response/1]).


-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").

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

role_request(Role)->
    GenId = 1,
    Request = loom_flow_lib:role_request(Role, GenId),
    loom_controller:broadcast_flow_mod(Request).

%% send requests to dp to get all config, status and stats from switch
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
%    loom_ofdp:send_ofp_msg(Pid, loom_flow_lib:queue_stats_request(any)),      ___ NEED TO REPORT BUG 
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

% Send packet to controller if tcp dst port = TcpPort
% Also send packet to output port in any case.
tap_port(Port1, Port2, Port3, TransportPort, Pid)->
    M = loom_flow_lib:tap_forward(Port1, Port2, Port3, TransportPort),
    loom_ofdp:send_ofp_msg(Pid, M),
    M0 = loom_flow_lib:tap_forward(Port2, Port1, Port3, TransportPort),
    loom_ofdp:send_ofp_msg(Pid, M0),
    M1 = loom_flow_lib:forward_mod(Port1,[Port2]),
    loom_ofdp:send_ofp_msg(Pid,M1),
    M2 = loom_flow_lib:forward_mod(Port2,[Port1]),
    loom_ofdp:send_ofp_msg(Pid,M2).  

% copies traffic between 1 and 2 to 3
test_tap() ->
    D = list_to_pid("<0.91.0>"),
    loom_ofdp_lib:clear(D),
    tap_port(1, 2, 3, 53, D).
    
% copies  udp traffic from Port 1 to Port2 and Port 3
% where srcIP is 10.0.2.60, 10.48.2.5 
test_tap2() ->
    [D|_] = loom_ofdp:get_all(default),
    loom_ofdp_lib:clear(D),
    IPv4Src1 = <<10:8,0:8,2:8,60:8>>,
    IPv4Src2 = <<10:8,48:8,2:8,5:8>>,
%    IPv4Src3 = <<10,48,33,190>>,
%    tap_port2(2,1,3, IPv4Src1, D),
%    tap_port2(2,1,3, IPv4Src2, D),
    tap_port2(1,2,controller, IPv4Src1, D),
    tap_port2(1,2,controller, IPv4Src2, D),      
    loom_ofdp_lib:forward(D,2,[1]), 
    loom_ofdp_lib:forward(D, 1,[2]).
    
% copies  DNS response traffic from Port 1 to Port2 and Port 3
tap_port2(Port1, Port2, Port3, IPv4Src, Pid) ->    
    M1 = loom_flow_lib:tap_dns_response(Port1, Port2, Port3, IPv4Src),
    loom_ofdp:send_ofp_msg(Pid, M1).
    
tap_all() ->
    [D|_] = loom_ofdp:get_all(default),
    loom_ofdp_lib:forward(D,2,[1, 3]), 
    loom_ofdp_lib:forward(D, 1,[2, 3]).

% copies all traffic from Port 1 to Port 2 and vice versa
test_link12() ->
    test_link(1, 2, []).
test_link(Port1, Port2, Ports)->
    [D|_] = loom_ofdp:get_all(default),
    dp_forward(D, Port1, [Port2|Ports]),
    dp_forward(D, Port2, [Port1|Ports]).
    
test_clear() ->
    [D|_] = loom_ofdp:get_all(default),
    dp_clear(D).

% usage my_controller:test_echo(<<"test">>).
test_echo(Data)->
    [D|_] = loom_ofdp:get_all(default),
    M = loom_flow_lib:echo_request(Data),
    loom_ofdp:send_ofp_msg(D, M).    

% send flow_stats_request
send_flow_stats_req(TableId) ->
    [D|_] = loom_ofdp:get_all(default),
    M1 = loom_ofmsg_lib:flow_stats_request(TableId),
    loom_ofdp:send_ofp_msg(D, M1).

%get stats_response 
show_response(Type) ->
    [Pid|_] = loom_ofdp_recv:get_all(default),
    loom_ofdp_recv:get_response(Pid, Type, self()),
    wait_for_reply().

wait_for_reply() ->
    receive
        {reply, Reply} ->
            io:format("Reply = ~p~n", [Reply]);
%        Message = Reply#ofp_message.body,
%        Stats = Message#ofp_flow_stats_reply.body,
%        [ flow_details(X) || X <- Stats];
        _ -> wait_for_reply()
    end.


flow_details(#ofp_flow_stats{table_id =TableId, duration_sec = DurationSec, priority= Priority,
                            idle_timeout = IdleTimeOut, hard_timeout = HardTimeOut, cookie = Cookie,
                            packet_count =PacketCount, byte_count = ByteCount, match = Match,
                            instructions= Instructions}) ->
    io:format("Flow_stats_reply: TableId = ~p, DurationSec = ~p, Priority = ~p, IdleTimeOut = ~p, HardTimeOut = ~p, Cookie = ~p, PacketCount = ~p, ByteCount = ~p, Match = ~p, Instructions =~p~n",
                [TableId, DurationSec, Priority, IdleTimeOut, HardTimeOut, Cookie, PacketCount, ByteCount, Match, Instructions]).