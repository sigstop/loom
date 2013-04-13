-module(loom_controller).

-behaviour(gen_server).

-define(DEFAULT_PORT, 6633).

-define(CLEAR_FLOWS,  #ofp_message{version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = delete, priority = 1, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = []}, instructions = [] } }).


-include_lib("../deps/of_protocol/include/of_protocol.hrl").
-include_lib("../deps/of_protocol/include/ofp_v4.hrl").
-include_lib("../deps/pkt/include/pkt.hrl").

-export([start/0,start/1,get_state/0,get_pid/0,get_connections/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start()->
    start(?DEFAULT_PORT).

start(Port)->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{port,Port}], []).

init(State)->
    [{port,Port}]=State,
    {ok,CtrlPid} = of_controller_v4:start(Port),
    NewState = State ++ [{pid,CtrlPid}],
    {ok,NewState}.
    

get_pid()->		   
    gen_server:call(?MODULE, get_pid).
    
get_state()->		   
    gen_server:call(?MODULE, get_state).

get_connections()->		   
    gen_server:call(?MODULE, get_connections).


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

    
%% callbacks
handle_call(get_state, _From, State) ->
    io:format("Loom controller = ~p~n", [State]),
    {reply, State, State};

handle_call(get_pid, _From, State) ->
    Pid = get_pid(State),
    io:format("Loom controller Pid = ~p~n", [Pid]),
    {reply, Pid, State};

handle_call(get_connections, _From, State) ->
    Connections = get_connections(State),
    io:format("Loom controller Connections = ~p~n", [Connections]),
    {reply, Connections, State};

handle_call(clear_all_flows, _From, State) ->
    Reply = clear_all_flows(State),
    io:format("Removing all flows from all switches!~n"),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% implementaion

get_pid(State)->
    lists:keyfind(pid,1,State).

get_connections(State)->
    {pid,CtrlPid} = get_pid(State),
    {ok,Connections} = of_controller_v4:get_connections(CtrlPid),
    Connections.


clear_all_flows(State)->
    CtrlPid = get_pid(State),
    Connections = get_connections(State),
    [Conn|_] = Connections,  %% TODO: handle all connections
    of_controller_v4:send(CtrlPid, Conn, ?CLEAR_FLOWS).
    
