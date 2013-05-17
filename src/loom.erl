-module(loom).

-export([start/0]).

start()->
    [code:add_pathz(Path) || Path <- filelib:wildcard("./deps/*/ebin")].
