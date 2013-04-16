-module(loom_flow_lib).

-include("../include/loom.hrl").

-export([gate_mod/2,tap_mod/2]).

gate_mod(Port1,Port2)->
    #ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 1, idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = [#ofp_field{ class = openflow_basic, name = in_port, value = <<Port1:32>>, has_mask = false}]}, instructions = [#ofp_instruction_write_actions{actions = [#ofp_action_output{ port = Port2, max_len=1400}]  }] } }.


tap_mod(InPort,OutPorts)->
    Actions = [#ofp_action_output{ port = Port, max_len=1400}||Port <- OutPorts],
    #ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 1,idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = [#ofp_field{ class = openflow_basic, name = in_port, value = <<InPort:32>>, has_mask = false}]}, instructions = [#ofp_instruction_write_actions{actions = Actions }] } }.
