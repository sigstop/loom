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

-module(flyshuttle_collector).

-behaviour(gen_server).

%% External API
-export([start_link/0,listen/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link()->
    gen_server:start_link(?MODULE, [], []).



listen(Interface)->
    {ok, _Pid} = epcap:start([{no_register, true},
                             {promiscuous, true},
                             {interface, Interface},
                             %% to work on ipv4-less interfaces
                             {no_lookupnet, true},
                             %% for ethernet-only (without taps and bridges)
                             {filter_incoming, true},
                             {filter, ""}]).

%%%-----------------------------------------------------------------------------
%%% gen_server callbacks
%%%-----------------------------------------------------------------------------

%% @private
init(State)->
    listen("en2"),
    {ok,State}.

-define(SWITCH_ID,255).
-define(PORT_ID,255).

%% @private
handle_call(_Message,_From,State)->
    {noreply, State}.

handle_cast(_Message,State)->
    {noreply, State}.

handle_info({packet, _DataLinkType, _Time, _Length, Frame},State) ->
    io:format("frame recieved\n"),
    LincPkt = linc_us4_packet:binary_to_record(Frame, ?SWITCH_ID, ?PORT_ID),
    io:format("packet recieved = ~w~n",[LincPkt]),
    {noreply, State};

handle_info(_Info, State) ->
    io:format("handle_info called\n"),
    {noreply, State}.

%% @private
terminate(_Reason, _State) -> terminated.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
