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

-module(loom_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,launch_controller/2,get_pid/0,get_children/0,start_controller/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API function.
%% ===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    {ok,Pid} = supervisor:start_link(?MODULE, []),
    register(loom_sup,Pid),
    {ok,Port} = application:get_env(loom,default_controller_port),
    Config = [{controller, default, Port}],
    [start_controller(Pid,Controller) || Controller <- Config],
    {ok,Pid}.

-spec start_controller( atom(), integer() ) -> ok| ignore | {error, term()}.
launch_controller(Name,Port) ->
    Pid = whereis(loom_sup),
    Config = [{controller, Name, Port}],
    [start_controller(Pid,Controller) || Controller <- Config],
    ok.

get_pid()->
    whereis(loom_sup).

get_children()->
    Pid = get_pid(),
    supervisor:which_children(Pid).

    

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

start_controller(Sup,{controller,Id,Port})->
    ControllerSup = {Id, {loom_controller_sup, start_link, [{controller,{Id,Port}}]},
		  permanent, 5000, supervisor, [loom_controller_sup]},
    supervisor:start_child(Sup,ControllerSup).
