%%------------------------------------------------------------------------------
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%-----------------------------------------------------------------------------
%%
%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox Inc
%% @doc An OpenFlow Controller and OF-Config Configuration Point Toolkit

-module(loom_ofdp).

-behaviour(gen_server).

-include("../include/loom.hrl").

-define(OFMSG,loom_ofmsg_lib).

-record(state, {pid, parent, listener, socket, address, port, sup, parser}).

%% API
-export([start_link/3,create/3,get_pid/1,send/2,send_ofp_msg/2,socket_closed/2,get_all/1,get_address/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Parent,Listener,Socket)->
    gen_server:start_link(?MODULE, [#state{parent=Parent,listener=Listener,socket=Socket}],[]).

create(Parent,Listener,Socket)->
    loom_ofdp_sup:start_child(Parent,Listener,Socket).

send(Pid,Msg) when is_pid(Pid) ->
    gen_server:cast(Pid,{send,Msg});
send(Name,Msg) when is_tuple(Name) ->
    Pid = get_pid(Name),
    send(Pid,Msg).

send_ofp_msg(Pid,OFPMsg) when is_pid(Pid) ->
    gen_server:cast(Pid,{send_ofp_msg,OFPMsg});
send_ofp_msg(Name,OFPMsg) when is_tuple(Name) ->
    Pid = get_pid(Name),
    send_ofp_msg(Pid,OFPMsg).


socket_closed(Pid,Socket)->
    gen_server:cast(Pid,{socket_closed,Socket}).

get_pid(Name) when is_tuple(Name)->
    global:whereis_name(Name).

get_all(ID)->
    LoomSupTree = loom:get_sup_tree(),
    get_ofdp_list(ID,LoomSupTree).

get_ofdp_list(ID,LoomSupTree)->
    Loom = lists:keyfind(ID,1,LoomSupTree),
    IDChildren = case Loom of
		      false ->
			  not_running;
		      {ID,_,Children} -> Children
		  end,
    OFDPL = case IDChildren of
		not_running ->
		    false;
		[] -> false;
		_ -> {OFDP,_Rest} = lists:partition(fun(X)->
							   [Name | _Rest] = tuple_to_list(X),
							   Name == loom_ofdp_sup end,IDChildren),
		     OFDP
	    end,
    Workers = case OFDPL of
		  [] -> false;
		  [{_,_,W}] -> W;
		  _ -> false
	      end,
    lists:foldl(fun(X,AccIn)->case X of
				  {_,Pid,worker,[loom_ofdp]} ->
				      [Pid|AccIn];
				  _ -> AccIn
			      end
		end,[],Workers).

get_address(Pid)->
    gen_server:call(Pid,get_address).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================


init([State])->
    Pid = self(),
    Socket = State#state.socket,
    {ok, {Address, Port}} = inet:peername(Socket),
    lager:info("Accepted connection from OFDatapath at ~p {~p,~p}",
	       [Socket, Address, Port]),
    Name = make_name(Address,Port),
    global:register_name(Name,self()),
    {ok, Parser} = ofp_parser:new(4),
    gen_server:cast(self(),recv),
    NewState = State#state{ pid = Pid, address = Address, port = Port, parser = Parser },
    {ok,NewState}.

handle_call(get_address, _From, State) ->
    Socket = State#state.socket,
    {ok, Address} = inet:peername(Socket),
    Reply = Address,
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    io:format("GOT UNKNOWN CALL REQUEST: ~p~n",[Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({socket_closed,Socket},State)->
    lager:info("Socket closed ~p",[Socket]),
    case Socket == State#state.socket of
	true ->
	    exit(socket_closed)
    end,
    {noreply,State};


handle_cast(recv,State)->
    Socket = State#state.socket,
    Parent = State#state.parent,
    Listener = State#state.listener,
    send(self(),hello),
    loom_ofdp_recv:create(Parent,Listener,self(),Socket),
    {noreply,State};

handle_cast({send,Msg},State)->
    send_msg(State,Msg),
    {noreply, State};

handle_cast({send_ofp_msg,OFPMsg},State)->
    send_ofp_msg_i(State,OFPMsg),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("GOT UNKNOWN CAST REQUEST: ~p",[Msg]),
    {noreply, State}.


handle_info(Info, State) ->
    io:format("GOT UNKNOWN INFO REQUEST: ~p~n",[Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

send_msg(State,Msg)->
    OFPMsg = ?OFMSG:Msg(),
    lager:info("Sending ~w",[OFPMsg]),
    send_ofp_msg_i(State,OFPMsg).

send_ofp_msg_i(State,OFPMsg)->
    Socket = State#state.socket,
    {ok, EncodedMsg} = of_protocol:encode(OFPMsg),
    ok = gen_tcp:send(Socket,EncodedMsg),
    inet:setopts(Socket, [{active, once}]).
    
make_name(Address,Port)->
    {Address, Port}.
    
get_name(State)->
    Address = State#state.address,
    Port = State#state.port,
    make_name(Address,Port).
    

