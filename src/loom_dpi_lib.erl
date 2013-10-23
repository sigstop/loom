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
    try
	Packet = pkt:decapsulate({ether,Data}),
	case Packet of 
	    [EthHeader,Header1,Header2,Payload] ->
		EthSrc = EthHeader#ether.shost,
		EthDst = EthHeader#ether.dhost,
		EthType = EthHeader#ether.type,
		lager:info("Ether Packet: EthSrc = ~18s, EthDSt = ~18s ~n",
			   [loom_util:binary_to_hex(EthSrc), loom_util:binary_to_hex(EthDst)]),
		lager:info("Ether Packet of type",[EthType]),
		ProtoType = case EthType of
				ipv4 ->
				    Header1#ipv4.p;
				_ -> unknown
			    end,
		lager:info("Network Packet of type",[ProtoType]),
		Result = case ProtoType of
			     udp -> inet_dns:decode(Payload);
			     _ -> unknown
			 end,
		case Result of
		    {ok,DnsRec} -> lager:info("DNS Packet from ~p to ~p: ~p~n",[Header1#ipv4.saddr,Header1#ipv4.daddr,DnsRec]),
				   [ Pid ! {dns_reply,DnsRec} || Pid <- Pids ];
		    
		    _ -> ok
		end;
	    _ -> 
		lager:info("No match dropped: ~p~n",[Packet])
	end
    catch
	Error ->
	    lager:info("Decapsulation Error:~p Data: ~p~n",[Error,Data])
    end.
