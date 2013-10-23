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

dns_reply(Pids,Data)->
    lager:info("Inspecting packet: ~p~n",[Data]),
    try
	Packet = pkt:decapsulate({ether,Data}),
	case Packet of 
	    [EthHeader,Header1,Header2,Payload] ->
		lager:info("Header1: ~p~n",[Header1]),
		lager:info("Header2: ~p~n",[Header2]),
		[ether|_] = tuple_to_list(EthHeader),
		[Type1|_] = tuple_to_list(Header1),
		[Type2|_] = tuple_to_list(Header2),
		lager:info("Network Packet of type ~p/~p~n",[Type1,Type2]),
		Result = case (Type1 == ipv4) and (Type2 == udp) of
			     true ->  
				      lager:info("Attempting inet_dns:decode(..) on ~p~n",[Payload]),
				      inet_dns:decode(Payload);
			     _ -> unknown
			 end,
		case Result of
		    {ok,DnsRec} -> lager:info("DNS Packet: ~p~n",[DnsRec]),
				   Match = match_reply(DnsRec),
				   lager:info("Match Value: ~p~n",[Match]),
				   Return = case Match of
						{error,_} -> error;
						ID -> {Header1#ipv4.daddr,ID}
					    end,
				   lager:info("Return Value: ~p~n",[Return]),
				   [ Pid ! {dns_reply,DnsRec} || Pid <- Pids ];
		    
		    _ -> lager:info("No match dropped: ~p~n",[Result])
		end;
	    _ -> 
		lager:info("No match dropped: ~p~n",[Packet])
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
	{_,_,_,_,_,ID,_,_,_} -> ID
    end;
match_reply(_) ->
    {error,bad_response}.
