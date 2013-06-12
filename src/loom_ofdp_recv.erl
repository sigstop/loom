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

-record(state, {pid, parent, listener, sender, socket, address, port, sup, parser}).

%% API
-export([start_link/4,create/4,send/2]).

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

%% ===================================================================
%% gen_server callbacks
%% ===================================================================


init([State])->
    Pid = self(),
    {ok, Parser} = ofp_parser:new(4),
    NewState = State#state{ pid = Pid, parser = Parser },
    gen_server:cast(self(),recv),
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
    lager:info("Waiting for data on: ~p", [Socket]),
    receive
	{tcp, Socket, Data} ->
	    lager:info("Received TCP data from ~p: ~p", [Socket, Data]),
	    {ok, NewParser, Messages} = ofp_parser:parse(Parser,Data),
	    lists:foreach(fun(Message) ->
				  lager:info("Received Message from ~p: ~w", [Socket, Message])
			  end, Messages),
	    inet:setopts(Socket,[{active, once}]),
	    recv(State#state{parser = NewParser});
	{tcp_closed, Socket} ->
	    Sender = State#state.sender,
            lager:info("Socket ~p closed", [Socket]),
	    loom_ofdp:socket_closed(Sender,Socket),
	    exit(socket_closed)
    end.
		
		    
    
