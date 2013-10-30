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

-module(loom_controller).

-behaviour(gen_server).

-include("../include/loom.hrl").

-record(state, {id, pid, port, sup, subscribers}).

-export([start_link/2,broken_call/0,get_id/1,start/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

get_id(Pid)->
    gen_server:call(Pid, get_id).

start(Name,Port)->
    Sup = loom_sup:get_pid(),
    Config = {controller, Name, Port},
    loom_sup:start_controller(Sup,Config).

start_link(ID,Port)->
    gen_server:start_link(?MODULE, [#state{id=ID,port=Port,subscribers=[]}], []).

init([State])->
    Pid = self(),
    NewState = State#state{pid = Pid},
    gen_server:cast(self(),listen),
    {ok,NewState}.
    
broken_call()->
    gen_server:call(?MODULE, broken_call).
    
%% callbacks
handle_call(broken_call, _From, State) ->
    fake_module:broken_call(),
    {reply, error, State};

handle_call(get_id, _From, State) ->
    {reply, State#state.id, State};

handle_call({message, Message},_From,State)->
    io:format("GOT MESSAGE: ~p~n",[Message]),
    {noreply, State};

handle_call({subscribe, packet_in_dns_reply}, From, State)->
    Subscribers = State#state.subscribers,
    NewSubscribers = [{packet_in_dns_reply,From}|Subscribers],
    NewState = State#state{subscribers = NewSubscribers},
    Reply = ok,
    {reply, Reply, NewState};

handle_call(Request, _From, State) ->
    io:format("GOT UNKNOWN CALL REQUEST: ~p~n",[Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(listen,State)->
    loom_c_listen:create(State#state.id,State#state.pid,State#state.port),
    {noreply, State};
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
%% Common helper functions
%%------------------------------------------------------------------------------

-spec create(atom) -> ets:tid().
create(ControllerID) ->
    ets:new(name(ControllerID), [named_table, public,
				 {read_concurrency, true}]).

-spec register_switch(atom(), atom(), pid() | ets:tid()) -> true.
register_switch(ControllerId, Name, Pid) ->
    true = ets:insert(name(ControllerId), {Name, Pid}).

%%------------------------------------------------------------------------------
%% Local helpers
%%------------------------------------------------------------------------------

name(ControllerId) ->
    list_to_atom(atom_to_list(ControllerId) ++ "_switches").
