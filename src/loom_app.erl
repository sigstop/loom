-module(loom_app).

-export[start/0].

start()->
    [code:add_pathz(Path) || Path <- filelib:wildcard("./deps/*/ebin")],
    [code:add_pathz(Path) || Path <- filelib:wildcard("./apps/*/ebin")],
    error_logger:tty(false),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssh),
    ok = application:start(xmerl),
    ok = application:start(mnesia),
    ok = application:start(syntax_tools),
    ok = application:start(compiler),
    ok = application:start(lager),
    ok = lager:set_loglevel(lager_console_backend, error),
    ok = application:start(loom).
