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
%% @doc Loom Executive

-module(loom_executive).

-behaviour(gen_server).

-include("../include/loom.hrl").

-export([start/0,start_link/0,get_state/0,get_pid/0,connect/1,
	 get_connections/0,get_connection/1,get_config/1,broken_call/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start()->
    start_link().

start_link()->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(State)->
    NewState = State ++ [{connections,[]}],
    {ok,NewState}.
    
broken_call()->
    gen_server:call(?MODULE, broken_call).
    
get_pid()->		   
    gen_server:call(?MODULE, get_pid).
    
get_state()->		   
    gen_server:call(?MODULE, get_state).

get_connections()->
    gen_server:call(?MODULE, get_connections).

connect(CapableSwitch)->		   
    gen_server:call(?MODULE, {connect,CapableSwitch}).

get_connection(ConnNumber)->		   
    gen_server:call(?MODULE, {get_connection,ConnNumber}).

get_config(ConnNumber)->		   
    gen_server:call(?MODULE, {get_config,ConnNumber}).
    
%% callbacks
handle_call(get_state, _From, State) ->
    io:format("Loom executive = ~p~n", [State]),
    {reply, State, State};

handle_call(get_pid, _From, State) ->
    Pid = get_pid(State),
    io:format("Loom executive Pid = ~p~n", [Pid]),
    {reply, Pid, State};

handle_call(get_connections, _From, State) ->
    Connections = get_connections(State),
    io:format("Loom executive Connctions = ~p~n", [Connections]),
    {reply, Connections, State};

handle_call({connect,CapableSwitch}, _From, State) ->
    Connection = connect(CapableSwitch,State),
    io:format("Loom executive Connection = ~p~n", [Connection]),
    Connections = get_connections(State),
    NewConnections = [Connection|Connections],
    NewState1 = lists:keydelete(connections,1,State),
    NewState2 = [{connections,NewConnections}|NewState1],
    {reply, Connection, NewState2};

handle_call({get_connection,ConnNumber}, _From, State) ->
    Connection = get_connection(ConnNumber,State),
    {reply, Connection, State};

handle_call({get_config,ConnNumber}, _From, State) ->
    Config = get_config(ConnNumber,State),
    {reply, Config, State};

handle_call(broken_call, _From, State) ->
    fake_module:broken_call(),
    {reply, error, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% implementaion

get_pid(State)->
    {pid,Pid} = lists:keyfind(pid,1,State),
    Pid.

get_connections(State)->
    {connections,Connections} = lists:keyfind(connections,1,State),
    Connections.

get_connection(ConnNum,State)->
    Connections = get_connections(State),
    Connection = lists:nth(ConnNum,Connections),
    Connection.

connect(CapableSwitch,_State)->
    {IP,PortCreds} = CapableSwitch,
    {ok,Connection} = enetconf_client:connect(IP,PortCreds),
    Connection.

get_config(ConnNumber,State)->
    Connection = get_connection(ConnNumber,State),
    {ok, Config} = enetconf_client:get_config(Connection, running),
    ErlConfig = enetconf_parser:convert(Config),
    ErlConfig.
    
