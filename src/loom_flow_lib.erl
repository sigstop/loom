-module(loom_flow_lib).

-include("../include/loom.hrl").

-export([forward_mod/2]).

forward_mod(InPort,OutPorts)->
   Actions = [ #ofp_action_output{ max_len = 64, port = OutPort} || OutPort <- OutPorts ],
   #ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 1,idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = [#ofp_field{ class = openflow_basic, name = in_port, value = <<InPort:32>>, has_mask = false}]}, instructions = [#ofp_instruction_apply_actions{actions = Actions }] } }.
