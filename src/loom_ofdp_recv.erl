%%------------------------------------------------------------------------------
%% Copyright 2013 Infoblox Inc.
%%
%%-----------------------------------------------------------------------------

%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox.com
%% @doc An OpenFlow Controller and OF-Config Configuration Point Toolkit
-module(loom_ofdp_recv).

-behaviour(gen_server).

-include("../include/loom.hrl").

-record(state, {pid, console, parent, listener, sender, socket, address, port, sup, parser, message_cache}).
                
-record(cache, {features_reply, echo_reply, get_config_reply, desc_reply, flow_stats_reply,
                aggregate_stats_reply, table_stats_reply, port_stats_reply, queue_stats_reply,
                group_stats_reply, group_desc_reply, group_features_reply, meter_features_reply,
                meter_config_reply, table_features_reply, port_desc_reply, get_async_reply, packetin::{[], []}}).
%% API
-export([start_link/4,create/4,send/2,set_console/2, get_eth_src_list/1, get_eth_dst_list/1]).

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
    
%% ===================================================================
%% gen_server callbacks
%% ===================================================================


init([State])->
    Pid = self(),
    {ok, Parser} = ofp_parser:new(4),
    NewState = State#state{ pid = Pid, parser = Parser, message_cache=#cache{packetin = {[],[]}}},
    gen_server:cast(self(),recv),
    io:format("in ofdp_recv init Pid = ~p~n", [self()]),    
    {ok,NewState}.

handle_call(Request, _From, State) ->
    io:format("GOT UNKNOWN CALL REQUEST: ~p~n",[Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(recv,State)->
    Listener = State#state.listener,
    Listener ! {new_ofdp_recv,self()},
    recv(State),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("GOT UNKNOWN CAST REQUEST: ~p",[Msg]),
    {noreply, State}.


handle_info(Info, State) ->
    io:format("GOT UNKNOWN INFO REQUEST: ~p~n",[Info]),
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
    lager:info("Waiting for data on: ~p", [Socket]),
    receive
	{tcp, Socket, Data} ->
	    lager:info("Received TCP data from ~p", [Socket]),
	    {ok, NewParser, Messages} = ofp_parser:parse(Parser,Data),
            NewMessageCache = process_messages(Messages, MessageCache, Socket),
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
	{set_console, ConsolePid} ->
	    recv(State#state{console = ConsolePid})
    end.		    

%%% [SN] process, cache and display OF message received by controller
process_messages([], MessageCache, _) ->
    MessageCache;
process_messages(Messages, MessageCache, Socket) ->
    [Message|Rest] = Messages,
    NewMessageCache = process_message(Message, MessageCache, Socket),
    process_messages(Rest, NewMessageCache, Socket).

process_message(#ofp_message{body = #ofp_features_reply
            {datapath_mac= DatapathMac, datapath_id = DatapathId}} =Message, MessageCache, Socket) ->
    io:format("Received ofp_features_reply from ~p:  DatapathMac = ~p, Datapath_id = ~p ~n",
        [Socket, DatapathMac, DatapathId]),
    MessageCache#cache{features_reply = Message};
process_message(#ofp_message{body = #ofp_get_config_reply
        {flags = Flags, miss_send_len = MissSendLen}} =Message, MessageCache, Socket) ->
    io:format("Received ofp_get_config_reply from ~p:  Flags = ~p, MissSendLen = ~p ~n",
        [Socket, Flags, MissSendLen]),
    MessageCache#cache{get_config_reply = Message};
process_message(#ofp_message{body = #ofp_desc_reply
        {flags = Flags, mfr_desc = MfrDesc, hw_desc = HwDesc, sw_desc = SwDesc,
         serial_num = SerialNum, dp_desc = DpDesc}} = Message, MessageCache, Socket) ->
    io:format("Received ofp_desc_reply from ~p:  Flags = ~p, Mfr_desc = ~p, Hw_desc = ~p,
            Sw_desc = ~p, Serial_num = ~p, Dp_desc = ~p~n",
            [Socket, Flags, MfrDesc, HwDesc, SwDesc, SerialNum, DpDesc]),
    MessageCache#cache{desc_reply = Message};   
process_message(#ofp_message{body = #ofp_flow_stats_reply{flags = Flags, body = Stats}} = Message,
        MessageCache, Socket) ->
    io:format("Received flow_stats_reply from ~p:  Flags = ~p Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{flow_stats_reply = Message};
process_message(#ofp_message{body = #ofp_aggregate_stats_reply{flags = Flags, packet_count = PacketCount,
        byte_count = ByteCount, flow_count = FlowCount}} = Message, MessageCache, Socket) ->
    io:format("Received aggregate_stats_reply from ~p:  Flags = ~p, PacketCount = ~p, ByteCount = ~p,
        FlowCount = ~p~n", [Socket, Flags, PacketCount, ByteCount, FlowCount]),
    MessageCache#cache{aggregate_stats_reply = Message};
process_message(#ofp_message{body = #ofp_table_stats_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket) ->
    io:format("Received table_stats_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{table_stats_reply = Message};      
process_message(#ofp_message{body = #ofp_port_stats_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket) ->
    io:format("Received ports_stats_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{port_stats_reply = Message};
process_message(#ofp_message{body = #ofp_queue_stats_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket) ->
    io:format("Received queue_stats_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{queue_stats_reply = Message};
process_message(#ofp_message{body = #ofp_group_stats_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket) ->
    io:format("Received group_stats_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{group_stats_reply = Message};
process_message(#ofp_message{body = #ofp_group_desc_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket) ->
    io:format("Received group_desc_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{group_desc_reply = Message};
process_message(#ofp_message{body = #ofp_group_features_reply{flags = Flags, types = Types,
        capabilities = Capabilities, max_groups = MaxGroups, actions =Actions }} = Message, MessageCache, Socket) ->
    io:format("Received group_features_reply from ~p:  Flags = ~p, Types = ~p, Capabilities = ~p,
        MaxGroups=~p, Actions=~p~n", [Socket, Flags, Types, Capabilities, MaxGroups, Actions]),
    MessageCache#cache{group_features_reply = Message};
process_message(#ofp_message{body = #ofp_meter_features_reply{flags = Flags, max_meter = MaxMeter,
        band_types = BandTypes, capabilities = Capabilities, max_bands = MaxBands, max_color = MaxColor }} = Message, MessageCache, Socket) ->
    io:format("Received meter_features_reply from ~p:  Flags = ~p, MaxMeter = ~p,
        BandTypes = ~p, Capabilities = ~p, MaxBands = ~p, MaxColor= ~p~n", [Socket, Flags, MaxMeter,
        BandTypes, Capabilities, MaxBands, MaxColor]),
    MessageCache#cache{meter_features_reply = Message};
process_message(#ofp_message{body = #ofp_meter_config_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket) ->
    io:format("Received meter_config_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{meter_config_reply = Message};
process_message(#ofp_message{body = #ofp_table_features_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket) ->
    io:format("Received table_features_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{table_features_reply = Message};
process_message(#ofp_message{body = #ofp_port_desc_reply{flags = Flags, body = Stats}} = Message, MessageCache, Socket) ->
    io:format("Received port_desc_reply from ~p:  Flags = ~p, Stats = ~p~n", [Socket, Flags, Stats]),
    MessageCache#cache{port_desc_reply = Message};    
process_message(#ofp_message{body = #ofp_get_async_reply
        { packet_in_mask = {[MPktInReason], [SPktInReason]},
        port_status_mask = {[MPortStatReason], [SPortStatReason]},
        flow_removed_mask = {[MFlowRemovedReason], [SFlowRemovedReason]}}}= Message, MessageCache, Socket) ->
    io:format("Received get_async_reply from ~p:[{~p, ~p}, {~p, ~p} , {~p, ~p}]~n",
        [Socket, MPktInReason, SPktInReason, MPortStatReason, SPortStatReason, MFlowRemovedReason, SFlowRemovedReason]),   
    MessageCache#cache{get_async_reply = Message};
process_message(#ofp_message{body = #ofp_packet_in
        {table_id = TableId, match = Match, reason = Reason, data = Data}}, MessageCache, _Socket) ->
%   io:format("Received packet_in from ~p:  TableId = ~p, Match = ~p, Reason = ~p Cookie =~p~n",
%        [Socket, TableId, Match, Reason, Cookie]),
    process_packetin(Reason, TableId, Match, Data, MessageCache);
process_message(Message, MessageCache, Socket) ->
    io:format("Received Message from ~p: ~p ~n", [Socket, Message]),
    MessageCache.    

%packetin on action 
%EthSrcList is a list of tuples of {EthSrc, Count}
%EthDstList is a list of tuples of {EthDst, Count}
% Max len of list is 100
process_packetin(action, _TableId, _Match, Data, #cache{packetin = {EthSrcList, EthDstList}} = MessageCache) ->
    try
        [EthHeader | _] = pkt:decapsulate(Data),
        EthSrc = EthHeader#ether.shost,
        EthDst = EthHeader#ether.dhost,
        case lists:keyfind(EthSrc, 1, EthSrcList) of 
            false ->
                NEthSrcList = lists:sublist([{EthSrc, 1} | EthSrcList], 100);
            {EthSrc, SCount} ->
                NEthSrcList = lists:keyreplace(EthSrc, 1, EthSrcList, {EthSrc, SCount+1})
        end,
        case lists:keyfind(EthDst, 1, EthDstList) of
            false ->
                NEthDstList = lists:sublist([{EthDst, 1} | EthDstList], 100);
            {EthDst, DCount} ->
                NEthDstList = lists:keyreplace(EthDst, 1, EthDstList, {EthDst, DCount+1})
        end,
        lager:info("packetin reason = action, EthSrc = ~18s, EthDSt = ~18s ~n",
            [binary_to_hex(EthSrc), binary_to_hex(EthDst)]),
        MessageCache#cache{packetin = {NEthSrcList, NEthDstList}}      
    catch
        E1:E2 ->
%            lager:error("Pkt decapsulate error: ~p:~p", [E1, E2]),
%            lager:error("Probably received malformed frame", []),
%            lager:error("With data: ~p", [Data]),
            MessageCache
    end;
%% packetin on table miss - right now doing nothing, so only proactive
process_packetin(nomatch, _TableId, _Match, _Data, MessageCache) ->
    lager:info("packetin reason = nomatch~n"),
    MessageCache;
process_packetin(Reason, _TableId, _Match, _Data, MessageCache) ->
    lager:info("packetin reason = ~p~n", [Reason]),
    MessageCache.
    
%% utilities
%%from of_controller_v4.erl
binary_to_hex(Bin) ->
    binary_to_hex(Bin, "").
binary_to_hex(<<>>, Result) ->
    Result;
binary_to_hex(<<B:8, Rest/bits>>, Result) ->
    Hex = erlang:integer_to_list(B, 16),
    NewResult = Result ++ ":" ++ Hex,
    binary_to_hex(Rest, NewResult).

% List consists of tuples {EthAdrs, Count}
print_eth_list([]) ->
    ok;
print_eth_list([{Eth, Count}|Rest]) ->
    io:format("Eth = ~18s, Count = ~p~n", [binary_to_hex(Eth), Count]),
    print_eth_list(Rest).
    
