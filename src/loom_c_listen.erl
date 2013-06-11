%%------------------------------------------------------------------------------
%% Copyright 2013 Infoblox Inc.
%%
%%-----------------------------------------------------------------------------

%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox.com
%% @doc An OpenFlow Controller and OF-Config Configuration Point Toolkit
-module(loom_c_listen).

-behaviour(gen_server).

-include("../include/loom.hrl").

-record(state, {pid, parent, port, socket, sup}).

%% API
-export([start_link/2,create/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Parent,Port)->
    gen_server:start_link(?MODULE, [#state{parent=Parent,port=Port}],[]).

create(ControllerId,Parent,Port)->
    loom_c_listen_sup:start_child(ControllerId,Parent,Port).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================


init([State])->
    Pid = self(),
    NewState = State#state{pid = Pid },
    gen_server:cast(self(),start),
    {ok,NewState}.

handle_call(Request, _From, State) ->
    io:format("GOT UNKNOWN CALL REQUEST: ~p~n",[Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(start,State)->
    lager:info("Listening on port ~p",[State#state.port]),
    Socket = listen(State),
    gen_server:cast(self(),accept),
    NewState = State#state{ socket = Socket },
    {noreply, NewState};

handle_cast(accept,State)->
    lager:info("Accepting on port ~p",[State#state.port]),
    accept(State),
    gen_server:cast(self(),accept),
    {noreply,State};
handle_cast(Msg, State) ->
    io:format("GOT UNKNOWN CAST REQUEST: ~p~n",[Msg]),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

listen(State)->
    Port = State#state.port,
    Opts = [binary, {packet, raw},
	    {active, once}, {reuseaddr, true}],
    {ok, Socket } = gen_tcp:listen(Port,Opts),
    Socket.
    
accept(State)->
    Socket = State#state.socket,
    Parent = State#state.parent,
    {ok, NewSocket } = gen_tcp:accept(Socket),
    loom_ofdp:create(Parent,self(),NewSocket),
    receive 
	{new_ofdp_recv,Pid} ->
	    ok = gen_tcp:controlling_process(NewSocket,Pid)
    end.
	    
