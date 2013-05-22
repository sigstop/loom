-module(loom_flow_lib).

-export([remove_all_flows_mod/0,
	 forward_mod/2,forward_mod/4,
	 match_forward_mod/3,
	 match_forward_mod/5,
	 drop_loops_mod/2,
	 drop_loops_mod1/2,
	 get_flow_table_message/1,
	 flow_stats_request/1,
	 aggregate_stats_request/1,
	 config_packet_in/1,
         role_request/2]).

-include("../include/loom.hrl").

remove_all_flows_mod()->
    #ofp_message{ type = flow_mod, 
		  body = #ofp_flow_mod{table_id = 0,
				       command = delete, 
				       priority = 1, 
				       cookie = <<0,0,0,0,0,0,0,10>>,
				       cookie_mask = <<0,0,0,0,0,0,0,0>>,
				       match = #ofp_match{fields = []}, 
				       instructions = [] } }.


 %%#ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 1,idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = [#ofp_field{ class = openflow_basic, name = in_port, value = <<InPort:32>>, has_mask = false}]}, instructions = [#ofp_instruction_apply_actions{actions = Actions }] } }.

forward_mod(InPort,OutPorts)->
    forward_mod(InPort,OutPorts,100,[]).
forward_mod(InPort,OutPorts,Priority,Flags)->
   Actions = actions_out_ports(OutPorts),
   #ofp_message{ version = 4, 
		 type = flow_mod, 
		 body = #ofp_flow_mod{
		   table_id = 0,
		   command = add, 
		   priority = Priority, 
		   flags = Flags, 
		   idle_timeout = 0, 
		   hard_timeout = 0,
		   cookie = <<0,0,0,0,0,0,0,10>>,
		   cookie_mask = <<0,0,0,0,0,0,0,0>>,
		   match = #ofp_match{fields = 
					  [#ofp_field{ class = openflow_basic, 
						       name = in_port, 
						       value = <<InPort:32>>, 
						       has_mask = false}]}, 
		   instructions = [#ofp_instruction_apply_actions{actions = Actions }] } }.

%match_forward_mod(InPort,EthDst,IPv4Dst,OutPorts)->
%    Match1 = match_port_eth_dst(InPort,EthDst),
%    Match2 = match_port_ipv4_dst(InPort,IPv4Dst),
%    Matches = [Match1,Match2],
%    Actions = actions_out_ports(OutPorts),
%    _FlowMod = match_action_mod(Matches,Actions).


drop_loops_mod(InPort,IPv4Dst)->
    Match = match_port_ipv4_dst(InPort,IPv4Dst),
    Actions = actions_out_ports([7]),
    #ofp_message{ version = 4, 
		  type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 200,idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = Match, instructions = [#ofp_instruction_apply_actions{actions = Actions} ] } }.
    

%% Working
match_forward_mod(InPort,EthDst,OutPorts)->
    match_forward_mod(InPort,EthDst,OutPorts,200,[]).
match_forward_mod(InPort,EthDst,OutPorts,Priority,Flags)->
    Match = match_port_eth_dst(InPort,EthDst),
    Actions = actions_out_ports(OutPorts),
    #ofp_message{ version = 4, 
		  type = flow_mod,
		  body = #ofp_flow_mod{table_id = 0,
				       command = add, 
				       priority = Priority,
				       flags=Flags, 
				       idle_timeout = 0,
				       hard_timeout = 0, 
				       cookie = <<0,0,0,0,0,0,0,10>>,
				       cookie_mask = <<0,0,0,0,0,0,0,0>>,
				       match = Match, 
				       instructions = [#ofp_instruction_apply_actions{actions = Actions } ]} }.

drop_loops_mod1(InPort,EthDst)->
    Match = match_port_eth_dst(InPort,EthDst),
    Actions = actions_out_ports([7]),
    #ofp_message{ version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = add, priority = 200,idle_timeout = 30000, hard_timeout = 60000, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = Match, instructions = [#ofp_instruction_apply_actions{actions = Actions } ]} }.

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
    #ofp_match{fields = [#ofp_field{name = in_port, value = <<InPort:32>>}]}.
%%                                              #ofp_field{name = eth_type, value = <<8,0>>},
%%                                              #ofp_field{name = ipv4_dst, value = IPv4Dst}]}.

get_flow_table_message(TableId)->
#ofp_message{
version = 4,
type = ofp_multipart_request,
body = #ofp_experimenter_request {
  flags = [],
  experimenter = 16#00748771,
  exp_type = 1,
  data =  term_to_binary({{table_id,TableId}}) }}.

% Change controller role: nochange, equal, master, slave
role_request(Role, GenerationID)->
#ofp_message{
version = 4,
type = ofp_role_request,
body = #ofp_role_request {
  role = Role,
  generation_id = GenerationID }}.

% Get flow stats based on Port, Cookie, Match  
flow_stats_request(TableId) ->
#ofp_message{
version = 4,
type =  ofp_multipart_request,
body = #ofp_flow_stats_request {
    table_id = TableId
    }}.

aggregate_stats_request(TableId) ->
#ofp_message{
version = 4,
type =  ofp_multipart_request,
body = #ofp_aggregate_stats_request {
    table_id = TableId }}.

% possible values [no_match, action, invalid_ttl]
config_packet_in(Param)->
#ofp_message{version = 4,
	     type = ofp_set_async,
	     body = #ofp_set_async{
	       packet_in_mask = {Param,Param},
	       port_status_mask = {[add, delete, modify],[add, delete, modify]},
	       flow_removed_mask = {[idle_timeout, hard_timeout, delete, group_delete], [idle_timeout, hard_timeout, delete, group_delete]}
	      }}.
