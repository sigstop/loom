%%------------------------------------------------------------------------------
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%-----------------------------------------------------------------------------
%%
%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox Inc
%% @doc Loom

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
