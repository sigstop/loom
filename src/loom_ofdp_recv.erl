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
%% @doc An OpenFlow Controller and OF-Config Configuration Point Toolkit

-module(loom_ofdp_recv).

-behaviour(gen_server).

-include("../include/loom.hrl").

-record(state, {pid, console, parent, listener, sender, socket, address, port, sup, parser, message_cache, subscribers}).
                
-record(cache, {features_reply, echo_reply, get_config_reply, desc_reply, flow_stats_reply,
                aggregate_stats_reply, table_stats_reply, port_stats_reply, queue_stats_reply,
                group_stats_reply, group_desc_reply, group_features_reply, meter_features_reply,
                meter_config_reply, table_features_reply, port_desc_reply, get_async_reply, packetin::{[], []}}).
%% API
-export([start_link/4,create/4,send/2,set_console/2, get_eth_src_list/1, get_eth_dst_list/1, get_all/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% ===================================================================
%% API functions
%% ===================================================================


start_link(Parent,Listener,Sender,Socket)->
    gen_server:start_link(?MODULE, [#state{parent=Parent,listener=Listener,sender=Sender,socket=Socket}],[]).

create(Parent,Listener,Sender,Socket)->
    loom_ofdp_recv_sup:start_child(Parent,Listener,Sender,Socket).

send(Pid,Msg)->
    gen_server:cast(Pid,{send,Msg}).

set_console(Pid,ConsolePid)->
    Pid ! {set_console,ConsolePid}.
    
get_eth_src_list(Pid)->
    Pid ! {get_eth_src_list}.

get_eth_dst_list(Pid)->
    Pid ! {get_eth_dst_list}.

get_all(ID)->
    LoomSupTree = loom:get_sup_tree(),
    get_ofdp_recv_list(ID,LoomSupTree).

get_ofdp_recv_list(ID,LoomSupTree)->
    Loom = lists:keyfind(ID,1,LoomSupTree),
    IDChildren = case Loom of
		      false ->
			  not_running;
		      {ID,_,Children} -> Children
		  end,
    OFDPL = case IDChildren of
		not_running ->
		    false;
		[] -> false;
		_ -> {OFDP,_Rest} = lists:partition(fun(X)->
							   [Name | _Rest] = tuple_to_list(X),
							   Name == loom_ofdp_recv_sup end,IDChildren),
		     OFDP
	    end,
    Workers = case OFDPL of
		  [] -> false;
		  [{_,_,W}] -> W;
		  _ -> false
	      end,
    lists:foldl(fun(X,AccIn)->case X of
				  {_,Pid,worker,[loom_ofdp_recv]} ->
				      [Pid|AccIn];
				  _ -> AccIn
			      end
		end,[],Workers).
    
%% ===================================================================
%% gen_server callbacks
%% ===================================================================


init([State])->
    Pid = self(),
    {ok, Parser} = ofp_parser:new(4),
    NewState = State#state{ pid = Pid, parser = Parser, message_cache=#cache{packetin = {[],[]}},
			  subscribers = []},
    gen_server:cast(self(),recv),
    lager:info("in ofdp_recv init Pid = ~p~n", [self()]),    
    {ok,NewState}.

handle_call({subscribe, {Pid, packet_in_dns_reply}},_From, State)->
    Subscribers = State#state.subscribers,
    NewSubscribers = [{packet_in_dns_reply,Pid}|Subscribers],
    NewState = State#state{subscribers = NewSubscribers},
    Reply = ok,
    {reply, Reply, NewState};
handle_call(Request, _From, State) ->
    lager:info("GOT UNKNOWN CALL REQUEST: ~p~n",[Request]),
    Reply = ok,
    {reply, Reply, State}.


handle_cast(recv,State)->
    Listener = State#state.listener,
    Listener ! {new_ofdp_recv,self()},
    recv(State),
    {noreply, State};


handle_cast(Msg, State) ->
    lager:info("GOT UNKNOWN CAST REQUEST: ~p",[Msg]),
    {noreply, State}.


handle_info(Info, State) ->
    lager:info("GOT UNKNOWN INFO REQUEST: ~p~n",[Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

recv(State) ->
    Socket = State#state.socket,
    Parser = State#state.parser,
    MessageCache = State#state.message_cache,
    Subscribers = State#state.subscribers,
    lager:info("Waiting for data on: ~p", [Socket]),
    receive
	{tcp, Socket, Data} ->
	    lager:info("Received TCP data from ~p", [Socket]),
	    {ok, NewParser, Messages} = ofp_parser:parse(Parser,Data),
            NewMessageCache = process_messages(Messages, MessageCache, Socket, Subscribers, Data),
	    inet:setopts(Socket,[{active, once}]),
	    recv(State#state{parser = NewParser, message_cache = NewMessageCache});
	{tcp_closed, Socket} ->
	    Sender = State#state.sender,
            lager:info("Socket ~p closed", [Socket]),
	    loom_ofdp:socket_closed(Sender,Socket),
	    exit(socket_closed);
        {get_eth_src_list} ->
            Cache = State#state.message_cache,
            {EthSrcList, _} = Cache#cache.packetin,
            print_eth_list(EthSrcList),
            recv(State);
        {get_eth_dst_list} ->
            Cache = State#state.message_cache,
            {_, EthDstList} = Cache#cache.packetin,
            print_eth_list(EthDstList),
            recv(State);
	{subscribe, {Pid, packet_in_dns_reply}} ->
	    NewSubscribers = [{packet_in_dns_reply,Pid}|Subscribers],
	    NewState = State#state{subscribers = NewSubscribers},
	    recv(NewState);
	{set_console, ConsolePid} ->
	    recv(State#state{console = ConsolePid})
    end.		    

%%% [SN] process, cache and display OF message received by controller
process_messages([], MessageCache, _, _, _) ->
    MessageCache;
process_messages(Messages, MessageCache, Socket, Subscribers, Data) ->
    [Message|Rest] = Messages,
    NewMessageCache = process_message(Message, MessageCache, Socket,Subscribers, Data),
    process_messages(Rest, NewMessageCache, Socket,Subscribers, Data).

process_message(#ofp_message{body = #ofp_features_reply
            {datapath_mac= DatapathMac, datapath_id = DatapathId}} =Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received ofp_features_reply from ~p:  DatapathMac = ~p, Datapath_id = ~p ~n",
        [Socket, DatapathMac, DatapathId]),
    MessageCache#cache{features_reply = Message};
process_message(#ofp_message{body = #ofp_get_config_reply
        {flags = Flags, miss_send_len = MissSendLen}} =Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received ofp_get_config_reply from ~p:  Flags = ~p, MissSendLen = ~p ~n",
        [Socket, Flags, MissSendLen]),
    MessageCache#cache{get_config_reply = Message};
process_message(#ofp_message{body = #ofp_desc_reply
        {flags = Flags, mfr_desc = MfrDesc, hw_desc = HwDesc, sw_desc = SwDesc,
         serial_num = SerialNum, dp_desc = DpDesc}} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received ofp_desc_reply from ~p:  Flags = ~p, Mfr_desc = ~p, Hw_desc = ~p,
            Sw_desc = ~p, Serial_num = ~p, Dp_desc = ~p~n",
            [Socket, Flags, MfrDesc, HwDesc, SwDesc, SerialNum, DpDesc]),
    MessageCache#cache{desc_reply = Message};   
process_message(#ofp_message{body = #ofp_flow_stats_reply{flags = Flags, body = Stats}} = Message,
        MessageCache, Socket, _Subscribers, _Raw) ->
    my_controller:stats_response(Stats),
%    lager:info("Received flow_stats_reply from ~p:  Flags = ~p Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{flow_stats_reply = Message};
process_message(#ofp_message{body = #ofp_aggregate_stats_reply{flags = Flags, packet_count = PacketCount,
        byte_count = ByteCount, flow_count = FlowCount}} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received aggregate_stats_reply from ~p:  Flags = ~p, PacketCount = ~p, ByteCount = ~p,
        FlowCount = ~p~n", [Socket, Flags, PacketCount, ByteCount, FlowCount]),
    MessageCache#cache{aggregate_stats_reply = Message};
process_message(#ofp_message{body = #ofp_table_stats_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received table_stats_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{table_stats_reply = Message};      
process_message(#ofp_message{body = #ofp_port_stats_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received ports_stats_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{port_stats_reply = Message};
process_message(#ofp_message{body = #ofp_queue_stats_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received queue_stats_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{queue_stats_reply = Message};
process_message(#ofp_message{body = #ofp_group_stats_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received group_stats_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{group_stats_reply = Message};
process_message(#ofp_message{body = #ofp_group_desc_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received group_desc_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{group_desc_reply = Message};
process_message(#ofp_message{body = #ofp_group_features_reply{flags = Flags, types = Types,
        capabilities = Capabilities, max_groups = MaxGroups, actions =Actions }} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received group_features_reply from ~p:  Flags = ~p, Types = ~p, Capabilities = ~p,
        MaxGroups=~p, Actions=~p~n", [Socket, Flags, Types, Capabilities, MaxGroups, Actions]),
    MessageCache#cache{group_features_reply = Message};
process_message(#ofp_message{body = #ofp_meter_features_reply{flags = Flags, max_meter = MaxMeter,
        band_types = BandTypes, capabilities = Capabilities, max_bands = MaxBands, max_color = MaxColor }} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received meter_features_reply from ~p:  Flags = ~p, MaxMeter = ~p,
        BandTypes = ~p, Capabilities = ~p, MaxBands = ~p, MaxColor= ~p~n", [Socket, Flags, MaxMeter,
        BandTypes, Capabilities, MaxBands, MaxColor]),
    MessageCache#cache{meter_features_reply = Message};
process_message(#ofp_message{body = #ofp_meter_config_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received meter_config_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{meter_config_reply = Message};
process_message(#ofp_message{body = #ofp_table_features_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received table_features_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{table_features_reply = Message};
process_message(#ofp_message{body = #ofp_port_desc_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket, _Subscribers, _Raw) ->
    lager:info("Received port_desc_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{port_desc_reply = Message};    
process_message(#ofp_message{body = #ofp_get_async_reply
        { packet_in_mask = {[MPktInReason], [SPktInReason]},
        port_status_mask = {[MPortStatReason], [SPortStatReason]},
        flow_removed_mask = {[MFlowRemovedReason], [SFlowRemovedReason]}}}= Message, MessageCache, Socket,_Subscribers, _Raw) ->
    lager:info("Received get_async_reply from ~p:[{~p, ~p}, {~p, ~p} , {~p, ~p}]~n",
        [Socket, MPktInReason, SPktInReason, MPortStatReason, SPortStatReason, MFlowRemovedReason, SFlowRemovedReason]),   
    MessageCache#cache{get_async_reply = Message};
process_message(#ofp_message{body = #ofp_packet_in
	{table_id = TableId, match = Match, reason = Reason, data = Data}}, MessageCache, _Socket,Subscribers, _Raw) ->
%   lager:info("Received packet_in from ~p:  TableId = ~p, Match = ~p, Reason = ~p Cookie =~p~n",
%        [Socket, TableId, Match, Reason, Cookie]),
    process_packetin(Reason, TableId, Match, Data, MessageCache, Subscribers);
process_message(Message, MessageCache, Socket,_Subscribers, Raw) ->
    lager:info("Why this? ..Received Data from ~p: ~p ~n", [Socket, Raw]),
    MessageCache.    

%packetin on action 
%EthSrcList is a list of tuples of {EthSrc, Count}
%EthDstList is a list of tuples of {EthDst, Count}
% Max len of list is 100
process_packetin(action, _TableId, _Match, Data, #cache{packetin = {EthSrcList, EthDstList}} = MessageCache,Subscribers) ->
   
    loom_dpi_lib:dns_reply(Subscribers,Data),
    MessageCache;     
process_packetin(nomatch, _TableId, _Match, _Data, MessageCache,_Subscribers) ->
    lager:info("packetin reason = nomatch~n"),
    MessageCache;
process_packetin(Reason, _TableId, _Match, _Data, MessageCache,_Subscribers) ->
    lager:info("packetin reason = ~p~n", [Reason]),
    MessageCache.
    


% List consists of tuples {EthAdrs, Count}
print_eth_list([]) ->
    ok;
print_eth_list([{Eth, Count}|Rest]) ->
    lager:info("Eth = ~18s, Count = ~p~n", [loom_util:binary_to_hex(Eth), Count]),
    print_eth_list(Rest).
    
