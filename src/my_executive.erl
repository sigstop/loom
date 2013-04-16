-module(my_executive).

-export([start/0,add_capable_switch/1,get_config/1,get_config_xml/1,print_config_xml/1]).


capable_switch(IP)->
    {IP,[{port, 1830}, {user, "linc"}, {password, "linc"}]}.

start()->
    loom_executive:start().


add_capable_switch(IP)->
    CapableSwitch = capable_switch(IP),
    loom_executive:connect(CapableSwitch).


get_config(IP)->
    Config = get_config_xml(IP),
    Result = enetconf_parser:convert(Config),
    {'rpc-reply',
     [{'message-id',_},{xmlns,_}],
     [{data,[],[ErlConfig]}]} = Result,
    ErlConfig.

get_config_xml(IP)->
    {ok,Connection} = enetconf_client:connect(IP,[{port, 1830}, {user, "linc"}, {password, "linc"}]),
    {ok, Config} = enetconf_client:get_config(Connection, running),
    Config.

print_config_xml(IP)->
    Config = get_config_xml(IP),
    io:format("Config = ~p~n",[Config]).
