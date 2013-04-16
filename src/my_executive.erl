-module(my_executive).

-export([start/0,add_capable_switch/1]).


capable_switch(IP)->
    {IP,[{port, 1830}, {user, "linc"}, {password, "linc"}]}.

start()->
    loom_executive:start().


add_capable_switch(IP)->
    CapableSwitch = capable_switch(IP),
    loom_executive:connect(CapableSwitch).
