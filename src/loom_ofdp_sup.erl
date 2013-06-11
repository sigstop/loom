%%------------------------------------------------------------------------------
%% Copyright 2013 Infoblox Inc.
%%
%%-----------------------------------------------------------------------------

%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox.com
%% @doc An OpenFlow Controller and OF-Config Configuration Point Toolkit
-module(loom_ofdp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_child/3,get_pid/1]).

%% Supervisor callbacks
-export([init/1]).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

start_link()->
    supervisor:start_link(?MODULE, []).

start_child(Parent,Listener,Socket) ->
    ControllerId = loom_controller:get_id(Parent),
    lager:info("ControllerId = ~p, Parent = ~p, Socket = ~w", [ControllerId,Parent,Socket]),
    Sup = get_pid(ControllerId),
    lager:info("Sup = ~p", [Sup]),
    supervisor:start_child(Sup, [Parent,Listener,Socket]).

get_pid(Id)->
    Parent = loom_controller_sup:get_pid(Id),
    List = supervisor:which_children(Parent),
    {?MODULE,Pid,supervisor,[?MODULE]} = 
	lists:keyfind(?MODULE,1,List),
    Pid.

%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------


init([])->
    Datapath = {loom_ofdp, {loom_ofdp, start_link, []},
		temporary, 5000, worker, [loom_ofdp]},
    Children = [Datapath],
    RestartStrategy = {simple_one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

