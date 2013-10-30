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
%% @doc Configuration Test

-module(config_test).

-compile([export_all]).

test1()->
    IP = "10.102.29.36",
    Port = "6633",
    Controller = {controller, [{id, ["Controller0"]}, {role, ["master"]},
			       {'ip-address', [IP]}, {port, [Port]}, {protocol, ["tcp"]}]},
    Attributes = [{xmlns, "urn:onf:of111:config:yang"}],
    Config = {'capable-switch', Attributes,
	      [{id, ["CapableSwitch0"]}, {'logical-switches',
					  [{'switch', [{id, ["LogicalSwitch0"]}, {'datapath-id', ["00:90:FB:37:71:6E:00:00"]},
						       {enabled, ["true"]}, {controllers, [Controller]}]}]}]},
    ssh:start(),
    {ok, C} = enetconf_client:connect("10.32.1.29", [{port, 1830}, {user, "linc"}, {password, "linc"}]),
%    {ok, RunningConfig} = enetconf_client:get_config(C, running),
    {ok, RunningConfig} = enetconf_client:get_config(C, startup),
    io:format("RunningConfig = ~p~n",[RunningConfig]),
%    Result = enetconf_client:edit_config(C, running, {xml, Config}),
    Result = enetconf_client:edit_config(C, startup, {xml, Config}),
    io:format("Edit complete...Result=~p~n",[Result]),
    timer:sleep(1000),
%    {ok, NewRunningConfig} = enetconf_client:get_config(C, running),
    {ok, NewRunningConfig} = enetconf_client:get_config(C, startup),
    io:format("NewRunningConfig = ~p~n",[NewRunningConfig]),
    NewRunningConfig.
    


test2()->
    IP = "10.102.29.36",
    Port = "6633",
    Controller = {controller, [{id, ["Controller0"]}, {role, ["master"]},
			       {'ip-address', [IP]}, {port, [Port]}, {protocol, ["tcp"]}]},
    Attributes = [{xmlns, "urn:onf:of111:config:yang"}],
    Config = {'capable-switch', Attributes,
	      [{id, ["CapableSwitch0"]}, {'logical-switches',
					  [{'switch', [{id, ["LogicalSwitch0"]}, {'datapath-id', ["00:90:FB:37:71:6E:00:00"]},
						       {enabled, ["true"]}, {controllers, [Controller]}]}]}]},
    ssh:start(),
    {ok, C} = enetconf_client:connect("10.192.168.152", [{port, 1830}, {user, "linc"}, {password, "linc"}]),
%    {ok, RunningConfig} = enetconf_client:get_config(C, running),
    {ok, RunningConfig} = enetconf_client:get_config(C, running),
    io:format("RunningConfig = ~p~n",[RunningConfig]),
    io:format("RunningErlangConfig = ~p~n",[enetconf_parser:convert(RunningConfig)]).
