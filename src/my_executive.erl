-module(my_executive).

-export([start/0,add_capable_switch/1,get_config/1]).


capable_switch(IP)->
    {IP,[{port, 1830}, {user, "linc"}, {password, "linc"}]}.

start()->
    loom_executive:start().


add_capable_switch(IP)->
    CapableSwitch = capable_switch(IP),
    loom_executive:connect(CapableSwitch).


get_config(IP)->
    {ok,Connection} = enetconf_client:connect(IP,[{port, 1830}, {user, "linc"}, {password, "linc"}]),
    {ok, Config} = enetconf_client:get_config(Connection, running),
    Result = enetconf_parser:convert(Config),
    {'rpc-reply',
     [{'message-id',_},{xmlns,_}],
     [{data,[],[ErlConfig]}]} = Result,
    ErlConfig.


