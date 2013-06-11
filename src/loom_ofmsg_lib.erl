%%------------------------------------------------------------------------------
%% Copyright 2013 Infoblox Inc.
%%
%%-----------------------------------------------------------------------------

%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox.com
%% @doc An OpenFlow Controller and OF-Config Configuration Point Toolkit
-module(loom_ofmsg_lib).

-include("../include/loom.hrl").

%% Message generators
-export([hello/0,
         table_miss_flow_mod/0,
         get_config_request/0,
         echo_request/0,
         echo_request/1,
         barrier_request/0,
         queue_get_config_request/0,
         features_request/0,
         remove_all_flows/0,
         group_mod/0,
         port_mod/0,
         set_config/0,
         role_request/0,
         desc_request/0,
         flow_stats_request/0,
         aggregate_stats_request/0,
         table_stats_request/0,
         port_stats_request/0,
         queue_stats_request/0,
         group_stats_request/0,
         group_desc_request/0,
         group_features_request/0
         ]).


%%%-----------------------------------------------------------------------------
%%% Message generators
%%%-----------------------------------------------------------------------------

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

barrier_request() ->
    message(#ofp_barrier_request{}).

queue_get_config_request() ->
    message(#ofp_queue_get_config_request{port = any}).

desc_request() ->
    message(#ofp_desc_request{}).

flow_stats_request() ->
    message(#ofp_flow_stats_request{table_id = all}).

aggregate_stats_request() ->
    message(#ofp_aggregate_stats_request{table_id = all}).

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

remove_all_flows() ->
    message(#ofp_flow_mod{command = delete}).

set_config() ->
    message(#ofp_set_config{miss_send_len = 16#ffff}).

group_mod() ->
    message(#ofp_group_mod{
               command  = add,
               type = all,
               group_id = 1,
               buckets = [#ofp_bucket{
                             weight = 1,
                             watch_port = 1,
                             watch_group = 1,
                             actions = [#ofp_action_output{port = 2}]}]}).

port_mod() ->
    message(#ofp_port_mod{port_no = 1,
                          hw_addr = <<0,17,0,0,17,17>>,
                          config = [],
                          mask = [],
                          advertise = [fiber]}).

role_request() ->
    message(#ofp_role_request{role = nochange, generation_id = 1}).

table_miss_flow_mod() ->
    Action = #ofp_action_output{port = controller},
    Instruction = #ofp_instruction_apply_actions{actions = [Action]},
    message(#ofp_flow_mod{table_id = 0,
                          command = add,
                          priority = 0,
                          instructions = [Instruction]}).

%%% Helpers --------------------------------------------------------------------

message(Body) ->
    #ofp_message{version = 4,
                 xid = get_xid(),
                 body = Body}.

get_xid() ->
    random:uniform(1 bsl 32 - 1).
