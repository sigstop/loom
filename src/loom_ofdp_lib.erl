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

-module(loom_ofdp_lib).

-compile([export_all]).
-export([link/3,forward/3,link_and_tap/4,link_and_tap2/4,clear/1]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

clear(Pid)->
    loom_ofdp:send(Pid,remove_all_flows).

forward(Pid,InPort,OutPorts)->
    Msg = loom_flow_lib:forward_mod(InPort,OutPorts),
    loom_ofdp:send_ofp_msg(Pid,Msg).

link(Pid,Port1,Port2)->
    forward(Pid,Port1,[Port2]),
    forward(Pid,Port2,[Port1]).

link_and_tap(Pid, Port1,Port2, TapPorts)->
    forward(Pid, Port1,[Port2 | TapPorts]),
    forward(Pid, Port2,[Port1 | TapPorts]).

link_and_tap2(Pid, Port1,Port2, TapPorts)->
    forward(Pid, Port1,[Port2]),
    forward(Pid, Port2,[Port1 | TapPorts]).

get_flow_table(Pid,TableId)->
    Msg = loom_ofmsg_lib:flow_stats_request(TableId),
    loom_ofdp:send_ofp_msg(Pid,Msg),
    loom:get_msg().
    
