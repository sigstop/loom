-module(loom_app).

-export[start/0].

start()->
    [code:add_pathz(Path) || Path <- filelib:wildcard("./deps/*/ebin")],
    [code:add_pathz(Path) || Path <- filelib:wildcard("./apps/*/ebin")],
    error_logger:tty(false),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssh),
    application:start(xmerl),
    application:start(mnesia),
    application:start(syntax_tools),
    application:start(compiler),
    application:start(lager),
    lager:set_loglevel(lager_console_backend, error),
    application:start(loom).
