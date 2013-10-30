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
%% @doc An OpenFlow Controller and OF-Config Configuration Point Toolkit

-module(loom_util).

-include("../include/loom.hrl").

-compile([export_all]).

%% utilities
%%from of_controller_v4.erl
binary_to_hex(Bin) ->
    binary_to_hex(Bin, "").
binary_to_hex(<<>>, Result) ->
    Result;
binary_to_hex(<<B:8, Rest/bits>>, Result) ->
    Hex = erlang:integer_to_list(B, 16),
    NewResult = Result ++ ":" ++ Hex,
    binary_to_hex(Rest, NewResult).
