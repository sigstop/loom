-module(loom_flow_lib).

-export([clear_all_flows_mod/0,forward_mod/2,match_forward_mod/4]).

-include("../include/loom.hrl").

clear_all_flows_mod()->
    #ofp_message{ type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = delete, priority = 1, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = []}, instructions = [] } }.


forward_mod(InPort,OutPorts)->
   Actions = actions_out_ports(OutPorts),
   #ofp_message{ type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 100,idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = [#ofp_field{ class = openflow_basic, name = in_port, value = <<InPort:32>>, has_mask = false}]}, instructions = [#ofp_instruction_apply_actions{actions = Actions }] } }.

match_forward_mod(InPort,EthDst,IPv4Dst,OutPorts)->
    Match1 = match_port_eth_dst(InPort,EthDst),
    Match2 = match_port_ipv4_dst(InPort,IPv4Dst),
    Matches = [Match1,Match2],
    Actions = actions_out_ports(OutPorts),
    _FlowMod = match_action_mod(Matches,Actions).


match_action_mod(Matches,Actions)->
#ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority =100,idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = Matches, instructions = [#ofp_instruction_apply_actions{actions = Actions }] } }.


actions_out_ports(OutPorts)->
    [ #ofp_action_output{ max_len = 64, port = OutPort} || OutPort <- OutPorts ].


% matches in_port and eth_dst
match_port_eth_dst(InPort,EthDst)->
    #ofp_match{fields = [#ofp_field{name = in_port, value = <<InPort:32>>}, 
			 #ofp_field{name = eth_dst, value = EthDst}]}.


% matches in_port and ipv4_dst
match_port_ipv4_dst(InPort,IPv4Dst)->
    #ofp_match{fields = [#ofp_field{name = in_port, value = <<InPort:32>>}, 
                                              #ofp_field{name = eth_type, value = <<8,0>>},
                                              #ofp_field{name = ipv4_dst, value = IPv4Dst}]}.
