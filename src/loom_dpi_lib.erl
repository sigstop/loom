%%------------------------------------------------------------------------------
%% Copyright 2013 Infoblox Inc.
%%
%%-----------------------------------------------------------------------------

%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox.com
%% @doc An OpenFlow Controller and OF-Config Configuration Point Toolkit
-module(loom_dpi_lib).

-include("../include/loom.hrl").

-compile([export_all]).

dns_reply(Subscribers,Data)->
    {MatchList,_Rest} = lists:partition(fun({Key,Value})->Key == packet_in_dns_reply end,Subscribers),
    Pids = lists:foldl(fun({Key,Value},AccIn)->[Value|AccIn] end,[],MatchList),
    try
	Packet = pkt:decapsulate({ether,Data}),
	case Packet of 
	    [EthHeader,Header1,Header2,Payload] ->
		[ether|_] = tuple_to_list(EthHeader),
		[Type1|_] = tuple_to_list(Header1),
		[Type2|_] = tuple_to_list(Header2),
		Result = case (Type1 == ipv4) and (Type2 == udp) of
			     true ->  
				 inet_dns:decode(Payload);
			     _ -> unknown
			 end,
		case Result of
		    {ok,DnsRec} -> 
			Match = match_reply(DnsRec),
			case Match of
			    {error,_} -> lager:info("No match dropped: ~p~n",[Match]);
			    ID ->
				R = list_to_tuple(binary_to_list(Header1#ipv4.daddr)),
				SendValue = {R,ID},
				lager:info("Sending: ~p~n",[SendValue]),
				[ Pid ! {dns_reply,SendValue} || Pid <- Pids ]
			end;
		    _ -> lager:info("No match dropped: ~p~n",[Result])
		end;
	    _ -> lager:info("No match dropped: ~p~n",[Packet])
	end
    catch
	Error ->
	    lager:info("Decapsulation Error:~p Data: ~p~n",[Error,Data])
    end.



match_reply({dns_rec,{dns_header,_,true,_,_,_,_,_,_,_},[{dns_query,_,a,in}],RRSet,_,_} )->
    Record = lists:keyfind(a,3,RRSet),
    case Record of
	false ->
	    {error,no_a_record};
	{dns_rr,_,a,_,_,_,ID,_,_,_} -> ID
    end;
match_reply(_) ->
    {error,bad_response}.
