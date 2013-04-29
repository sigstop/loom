-module(loom_flow_lib).

-include("../include/loom.hrl").

-export([gate_mod/3,tap_mod/3]).

gate_mod(Port1,Port2,Seq)->
    #ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 1, idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = [#ofp_field{ class = openflow_basic, name = in_port, value = <<Port1:32>>, has_mask = false}]}, instructions = [#ofp_instruction_write_actions{actions = [#ofp_action_output{ seq = Seq, port = Port2, max_len=1400}]  }] } }.


tap_mod(InPort,OutPorts,Seq)->
% loop that worked at ONS
%    [Outport1|Rest] = OutPorts,
%    [Outport2|_] = Rest,
%    Action1 = #ofp_action_output{ seq = Seq, port = Outport1, max_len=1400},
%    Action2 = #ofp_action_output{ seq = Seq+1, port = Outport2, max_len=1400},
%    Actions = [Action1,Action2],
%%    Actions = [#ofp_action_output{ seq = Seq, port = Port, max_len=1400}||Port <- OutPorts],
%    #ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 1,idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = [#ofp_field{ class = openflow_basic, name = in_port, value = <<InPort:32>>, has_mask = false}]}, instructions = [#ofp_instruction_write_actions{actions = Actions }] } }.

% SN: Uses #ofp_instruction_apply_actions instead of ofp_instruction_write_actions as only one action of a type are allowed with ofp_instruction_write_actions [5.10]
   [Outport1|Rest] = OutPorts,
   [Outport2|_] = Rest,
   Action1 = #ofp_action_output{ port = Outport1},
   Action2 = #ofp_action_output{ port = Outport2},
   Actions = [Action1,Action2],
   #ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 1,idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = [#ofp_field{ class = openflow_basic, name = in_port, value = <<InPort:32>>, has_mask = false}]}, instructions = [#ofp_instruction_apply_actions{actions = Actions }] } }.
