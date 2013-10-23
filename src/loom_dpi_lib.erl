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
		[ether|_] = tuple_to_list(EthHeader),
		[Type1|_] = tuple_to_list(Header1),
		[Type2|_] = tuple_to_list(Header2),
		lager:info("Network Packet of type ~p/~p~n",[Type1,Type2]),
		Result = case (Type1 == ipv4) and (Type2 == udp) of
			     true ->  IPHeader = pkt:ipv4(Header1),
				      UDPHeader = pkt:udp(Header2),
				      lager:info("IP Header: ~p~n",[IPHeader]),
				      lager:info("UDP Header: ~p~n",[UDPHeader]),
				      lager:info("Attempting inet_dns:decode(..) on ~p~n",[Payload]),
				      inet_dns:decode(Payload);
			     _ -> unknown
			 end,
		case Result of
		    {ok,DnsRec} -> lager:info("DNS Packet from ~p to ~p: ~p~n",[Header1#ipv4.saddr,Header1#ipv4.daddr,DnsRec]),
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
