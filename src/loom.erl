%%------------------------------------------------------------------------------
%% Copyright 2013 Infoblox Inc.
%%
%%-----------------------------------------------------------------------------

%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox.com
%% @doc An OpenFlow Controller and OF-Config Configuration Point Toolkit
-module(loom).

-behavior(application).

%% API
-export([get_sup_tree/0,get_msg/0]).

%% Application callbacks
-export([start/2,
	 stop/1]).

%% Common helpers
-export([create/1]).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

get_sup_tree()->
    Children = loom_sup:get_children(),
    get_sup_tree(Children,[]).

get_sup_tree([],Acc)->
    Acc;
get_sup_tree([{_Name,_ChildPid,worker,_Implementation} = Child|Rest],Acc)->
    get_sup_tree(Rest,Acc ++ [Child]);
get_sup_tree([Child|Rest],Acc)->
    {Name,ChildPid,_Type,_Implementation} = Child,
    NewAcc = case supervisor:which_children(ChildPid) of 
		 ok -> get_sup_tree(Rest,Acc ++ [{Name,Child}]);
		 Children ->
		     Acc ++ [{Name,Child,get_sup_tree(Children,[])}]
    end,
    get_sup_tree(Rest,NewAcc).


get_msg()->
    receive X -> io:format("~p~n",[X])
    after 500 -> no_reply 
    end.
    
    
    


%%------------------------------------------------------------------------------
%% Application callbacks
%%------------------------------------------------------------------------------

%% @doc Starts the application.
-spec start(any(), any()) -> {ok, pid()}.
start(StartType, StartArgs) ->
    lager:info("Starting Loom with Type ~p and Args ~p",[StartType,StartArgs]),
    loom_sup:start_link().

%% @doc Stops the application.
-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%%------------------------------------------------------------------------------
%% Common Loom helper functions
%%------------------------------------------------------------------------------

-spec create(atom()) -> ets:tid().
create(ControllerId) ->
    ets:new(name(ControllerId), [named_table, public,
				 {read_concurrency, true}]).

-spec delete(atom()) -> true.
delete(ControllerId) ->
    true = ets:delete(name(ControllerId)).

-spec register(atom(), atom(), pid() | ets:tid()) -> true.
register(ControllerId, Name, Pid) ->
    true = ets:insert(name(ControllerId), {Name, Pid}).

-spec lookup(atom(), atom()) -> term().
lookup(ControllerId, Name) ->
    case ets:lookup(name(ControllerId), Name) of
        [{Name, Pid}] ->
            Pid;
        [] ->
            undefined
    end.


%%start()->
%%    [code:add_pathz(Path) || Path <- filelib:wildcard("./deps/*/ebin")].

%%------------------------------------------------------------------------------
%% Local helpers
%%------------------------------------------------------------------------------

name(ControllerId) ->
    list_to_atom("loom_controller_" ++ atom_to_list(ControllerId)).
