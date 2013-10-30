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

-module(loom_controller_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,get_pid/1,get_children/1]).

%% Supervisor callbacks
-export([init/1]).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec start_link(term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Child)->
    supervisor:start_link(?MODULE, [Child]).

get_pid(Id)->
    Parent = loom_sup:get_pid(),
    List = supervisor:which_children(Parent),
    {Id,Pid,supervisor,[loom_controller_sup]} = lists:keyfind(Id,1,List),
    Pid.

get_children(Id)->
    Pid = get_pid(Id),
    supervisor:which_children(Pid).

%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------


init([{controller,{Id,Port}}])->
    Controller = {loom_controller, {loom_controller, start_link, [Id,Port]},
		  permanent, 5000, worker, [loom_controller]},
    ListenSup = {loom_c_listen_sup, {loom_c_listen_sup, start_link, []},
		 permanent, 5000, supervisor, [loom_c_listen_sup]},
    DatapathSup = {loom_ofdp_sup, {loom_ofdp_sup, start_link, []},
		   permanent, 5000, supervisor, [loom_ofdp_sup]},
    DatapathRecvSup = {loom_ofdp_recv_sup, {loom_ofdp_recv_sup, start_link, []},
		   permanent, 5000, supervisor, [loom_ofdp_recv_sup]},
    {ok, {{rest_for_one, 5, 10}, [Controller,ListenSup,DatapathSup,DatapathRecvSup]}}.
