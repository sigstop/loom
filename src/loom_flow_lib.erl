-module(loom_flow_lib).

-export([
         hello/0, features_request/0, echo_request/0, echo_request/1, get_config_request/0,
         desc_request/0, flow_stats_request/1, aggregate_stats_request/1, table_stats_request/0,
         port_stats_request/0, queue_stats_request/0, group_stats_request/0, group_desc_request/0,
         group_features_request/0, meter_stats_request/1, meter_config_request/1, meter_features_request/0,
         table_features_request/1, port_desc_request/0, queue_get_config_request/1, get_async_config/0,
         barrier_request/0,
         remove_all_flows_mod/0,forward_mod/2,forward_mod/4, get_flow_table_message/1,
	 match_forward_mod/3, match_forward_mod/5, drop_loops_mod/2, drop_loops_mod1/2,
	 config_packet_in/2, role_request/2, table_miss_flow_mod/1]).

-include("../include/loom.hrl").

% match everything, and priority = 0
table_miss_flow_mod(TableId) ->
    Action = #ofp_action_output{port = controller},
    Instruction = #ofp_instruction_apply_actions{actions = [Action]},
    message(#ofp_flow_mod{table_id = TableId,
                          command = add,
                          priority = 0,
                          instructions = [Instruction]}).

remove_all_flows_mod() ->
    message(#ofp_flow_mod{command = delete}).

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

% possible values [no_match, action, invalid_ttl]
config_packet_in(Param1, Param2)->
#ofp_message{version = 4,
            xid = get_xid(),
	     body = #ofp_set_async{
	       packet_in_mask = {Param1,Param2},
	       port_status_mask = {[add, delete, modify],[add, delete, modify]},
	       flow_removed_mask = {[idle_timeout, hard_timeout, delete, group_delete], [idle_timeout, hard_timeout, delete, group_delete]}
	      }}.

%% messages from controller to switch with responses

hello() ->
    message(#ofp_hello{}).

features_request() ->
    message(#ofp_features_request{}).
    
echo_request() ->
    echo_request(<<>>).
echo_request(Data) ->
    message(#ofp_echo_request{data = Data}).

get_config_request() ->
    message(#ofp_get_config_request{}).

desc_request() ->
    message(#ofp_desc_request{}).

flow_stats_request(TableId) ->
    message(#ofp_flow_stats_request{table_id = TableId}).

aggregate_stats_request(TableId) ->
    message(#ofp_aggregate_stats_request{table_id = TableId}).

table_stats_request() ->
    message(#ofp_table_stats_request{}).

port_stats_request() ->
    message(#ofp_port_stats_request{port_no = any}).

queue_stats_request() ->
    message(#ofp_queue_stats_request{port_no = any, queue_id = all}).

group_stats_request() ->
    message(#ofp_group_stats_request{group_id = all}).

group_desc_request() ->
    message(#ofp_group_desc_request{}).

group_features_request() ->
    message(#ofp_group_features_request{}).

% MeterId = interger(), slowpath, controller, all
meter_stats_request(MeterId) ->
    message(#ofp_meter_stats_request{meter_id = MeterId}).

% MeterId = interger(), slowpath, controller, all
meter_config_request(MeterId) ->
    message(#ofp_meter_config{meter_id = MeterId}).

meter_features_request() ->
    message(#ofp_meter_features_request{}).
    
table_features_request(TableId) ->
    message(#ofp_table_features{table_id = TableId}).

port_desc_request() ->
    message(#ofp_port_desc_request{}).

queue_get_config_request(PortNum) ->
    message(#ofp_queue_get_config_request{port=PortNum}).

% Change controller role: nochange, equal, master, slave
role_request(Role, GenerationID)->
    message(#ofp_role_request{role = Role, generation_id = GenerationID}).

barrier_request() ->
    message(#ofp_barrier_request{}).

queue_get_config_request() ->
    message(#ofp_queue_get_config_request{port = any}).
    
get_async_config() ->
    message(#ofp_get_async_request{}).

%%% Helpers --------------------------------------------------------------------
%%% just started this cleanup
message(Body) ->
    #ofp_message{version = 4,
                 xid = 1,
                 body = Body}.
%%% instead of random, we could use now
get_xid() ->
    random:uniform(1 bsl 32 - 1).

