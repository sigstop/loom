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
%% @doc Demonstration Script

-module(demo_script).


-compile([export_all]).


-include_lib("../deps/of_protocol/include/of_protocol.hrl").
-include_lib("../deps/of_protocol/include/ofp_v4.hrl").
-include_lib("../deps/pkt/include/pkt.hrl").

init()->ok.
    
start()->
    {ok,CtrlPid} = of_controller_v4:start(6633),
    CtrlPid.



add_flow1(CtrlPid)->
    {ok,[Conn|_]} = of_controller_v4:get_connections(CtrlPid),
    FlowMod1 =  #ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 1, idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = [#ofp_field{ class = openflow_basic, name = in_port, value = <<5:32>>, has_mask = false}]}, instructions = [#ofp_instruction_write_actions{actions = [#ofp_action_output{ port = 6, max_len=1400}]  }] } },
    of_controller_v4:send(CtrlPid, Conn, FlowMod1),
    FlowMod2 =  #ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 1, idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = [#ofp_field{ class = openflow_basic, name = in_port, value = <<6:32>>, has_mask = false}]}, instructions = [#ofp_instruction_write_actions{actions = [#ofp_action_output{ port = 5, max_len=1400}]  }] } },
    of_controller_v4:send(CtrlPid, Conn, FlowMod1),
    of_controller_v4:send(CtrlPid, Conn, FlowMod2).

add_tap(CtrlPid)->
    TapFlowMod =  #ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 1, idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = [#ofp_field{ class = openflow_basic, name = in_port, value = <<5:32>>, has_mask = false}]}, instructions = [#ofp_instruction_write_actions{actions = [#ofp_action_output{ port = 7, max_len=1400}]  }] } },
    {ok,[Conn|_]} = of_controller_v4:get_connections(CtrlPid),
    of_controller_v4:send(CtrlPid, Conn, TapFlowMod).

clear_all_flows(CtrlPid)->
    RemoveFlows =  #ofp_message{version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = delete, priority = 1, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = []}, instructions = [] } },
    {ok,[Conn|_]} = of_controller_v4:get_connections(CtrlPid),
    of_controller_v4:send(CtrlPid, Conn, RemoveFlows).



    
