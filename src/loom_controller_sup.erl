%%------------------------------------------------------------------------------
%% Copyright 2013 Infoblox Inc.
%%
%%-----------------------------------------------------------------------------

%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox.com
%% @doc An OpenFlow Controller and OF-Config Configuration Point Toolkit
-module(loom_controller_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,get_pid/1,get_children/1]).

%% Supervisor callbacks
-export([init/1]).

%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

-spec start_link(term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Child)->
    supervisor:start_link({local, loom_controller},?MODULE, [Child]).

get_pid(Id)->
    Parent = loom_sup:get_pid(),
    List = supervisor:which_children(Parent),
    {Id,Pid,supervisor,[loom_controller_sup]} = lists:keyfind(Id,1,List),
    Pid.

get_children(Id)->
    Pid = get_pid(Id),
    supervisor:which_children(Pid).

%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------


init([{controller,{Id,Port}}])->
    Controller = {loom_controller, {loom_controller, start_link, [Id,Port]},
		  permanent, 5000, worker, [loom_controller]},
    ListenSup = {loom_c_listen_sup, {loom_c_listen_sup, start_link, []},
		 permanent, 5000, supervisor, [loom_c_listen_sup]},
    DatapathSup = {loom_ofdp_sup, {loom_ofdp_sup, start_link, []},
		   permanent, 5000, supervisor, [loom_ofdp_sup]},
    DatapathRecvSup = {loom_ofdp_recv_sup, {loom_ofdp_recv_sup, start_link, []},
		   permanent, 5000, supervisor, [loom_ofdp_recv_sup]},
    {ok, {{rest_for_one, 5, 10}, [Controller,ListenSup,DatapathSup,DatapathRecvSup]}}.
