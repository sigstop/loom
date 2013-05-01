-module(loom_controller).

-behaviour(gen_server).

-include("../include/loom.hrl").

-export([start/0,start/1,start_link/0,start_link/2,get_state/0,get_pid/0,get_connections/0,
	remove_all_flows/0,broadcast_flow_mod/1,broken_call/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start()->
    start_link().

start(Port)->
    start_link(Port,?DEFAULT_OFP_VERSION).

start_link()->
    start_link(?DEFAULT_CNTL_PORT,?DEFAULT_OFP_VERSION).

start_link(Port,OFProtocolVersion)->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{port,Port},{ofp_ver,OFProtocolVersion}], []).

init(State)->
    Port = get_port(State),
    OFPVer = get_ofp_ver(State),
    ControllerModule = case OFPVer of
			   1.3 -> of_controller_v4;
			   1.2 -> of_controller
		       end,
    {ok,CtrlPid} = ControllerModule:start(Port),
    NewState = State ++ [{pid,CtrlPid},{cntrl_mod,ControllerModule}],
    {ok,NewState}.
    
broken_call()->
    gen_server:call(?MODULE, broken_call).
    
get_pid()->		   
    gen_server:call(?MODULE, get_pid).
    
get_state()->		   
    gen_server:call(?MODULE, get_state).

get_connections()->		   
    gen_server:call(?MODULE, get_connections).

remove_all_flows()->
    gen_server:call(?MODULE, remove_all_flows).

broadcast_flow_mod(Flow)->
    gen_server:call(?MODULE, {broadcast_flow_mod,Flow}).
    
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

handle_call({broadcast_flow_mod, FlowMod} , _From, State) ->
    Reply = broadcast_flow_mod(State,FlowMod),
    io:format("Adding a flow mod to all switches!~n"),
    {reply, Reply, State};

handle_call(remove_all_flows, _From, State) ->
    Reply = remove_all_flows(State),
    io:format("Removing all flows from all switches!~n"),
    {reply, Reply, State};

handle_call(broken_call, _From, State) ->
    fake_module:broken_call(),
    {reply, error, State};

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

get_state_value(State,Name)->
    {Name,Value} = lists:keyfind(Name,1,State),
    Value.

get_pid(State)->
    get_state_value(State,pid).

get_ofp_ver(State)->
    get_state_value(State,ofp_ver).

get_port(State)->
    get_state_value(State,port).

get_cntrl_mod(State)->
    get_state_value(State,cntrl_mod).

get_connections(State)->
    CtrlPid = get_pid(State),
    CntrlMod = get_cntrl_mod(State),
    {ok,Connections} = CntrlMod:get_connections(CtrlPid),
    Connections.


broadcast_flow_mod(State,FlowMod)->
    CtrlPid = get_pid(State),
    Connections = get_connections(State),
    [Conn|_] = Connections,  %% TODO: handle all connections
    CntrlMod = get_cntrl_mod(State),
    CntrlMod:send(CtrlPid, Conn, FlowMod).

remove_all_flows(State)->
    CntrlMod = get_cntrl_mod(State),
    FlowMod = CntrlMod:remove_all_flows(),
    broadcast_flow_mod(State,FlowMod).


    
