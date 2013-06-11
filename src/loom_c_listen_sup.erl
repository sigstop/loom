%% Copyright 2013 Infoblox Inc.
%%
%%-----------------------------------------------------------------------------

%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox.com
%% @doc An OpenFlow Controller and OF-Config Configuration Point Toolkit
-module(loom_c_listen_sup).

-behaviour(supervisor).

%% API
-export([start_child/3,start_link/0,get_pid/1,get_children/1]).


%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API function.
%% ===================================================================

start_link()->
    supervisor:start_link(?MODULE,[]).

start_child(ControllerId,Parent,Port) ->
    Sup = get_pid(ControllerId),
    supervisor:start_child(Sup, [Parent,Port]).

get_children(Id)->
    Pid = get_pid(Id),
    supervisor:which_children(Pid).

get_pid(Id)->
    Parent = loom_controller_sup:get_pid(Id),
    List = supervisor:which_children(Parent),
    {loom_c_listen_sup,Pid,supervisor,[loom_c_listen_sup]} = 
	lists:keyfind(loom_c_listen_sup,1,List),
    Pid.

init([]) ->
    lager:info("Made it!"),
    Listener = {loom_c_listen, {loom_c_listen, start_link, []},
		temporary, 5000, worker, [loom_c_listen]},
    Children = [Listener],
    RestartStrategy = {simple_one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.
