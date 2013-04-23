-module(my_executive).

-export([start/0,add_capable_switch/1,get_config/1,get_config_xml/1,print_config_xml/1,
	tilera_linc/0,linc3/0,linc4/0,pi_linc/0]).

-define(TILERA,{"10.192.168.152",[{port, 1831}, {user, "linc"}, {password, "linc"}]}).
-define(LINC3,{"10.192.168.152",[{port, 1830}, {user, "linc"}, {password, "linc"}]}).
-define(LINC4,{"10.192.168.158",[{port, 1830}, {user, "linc"}, {password, "linc"}]}).
-define(PI,{"10.192.168.163",[{port, 1830}, {user, "linc"}, {password, "linc"}]}).


capable_switch(IP)->
    {IP,[{port, 1830}, {user, "linc"}, {password, "linc"}]}.

start()->
    loom_executive:start().


add_capable_switch(IP)->
    CapableSwitch = capable_switch(IP),
    loom_executive:connect(CapableSwitch).

tilera_linc()->
    ?TILERA.

linc3()->
    ?LINC3.

linc4()->
    ?LINC4.

pi_linc()->
    ?PI.

get_config(CapableSwitch)->
    Config = get_config_xml(CapableSwitch),
    Result = enetconf_parser:convert(Config),
    {'rpc-reply',
     [{'message-id',_},{xmlns,_}],
     [{data,[],[ErlConfig]}]} = Result,
    ErlConfig,
    io:format("Config = ~p~n",[ErlConfig]).

get_config_xml(CapableSwitch)->
    {IP,Creds}=CapableSwitch,
    {ok,Connection} = enetconf_client:connect(IP,Creds),
    {ok, Config} = enetconf_client:get_config(Connection, running),
    Config.

print_config_xml(CapableSwitch)->
    Config = get_config_xml(CapableSwitch),
    io:format("Config = ~p~n",[Config]).
