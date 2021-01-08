%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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
%%--------------------------------------------------------------------

-module(emqx_hook_test_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-include("emqx_hook_test.hrl").

-export([ start/2
	  , stop/1
	]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_hook_test_sup:start_link(),
    emqx_hook_test:load(load_env()),
    {ok, Sup}.

stop(_State) ->
    emqx_hook_test:unload().

load_env() ->
    {ok, Timeout} = application:get_env(?APP, query_timeout),
    Type = proplists:get_value(type, application:get_env(?APP, server, [])),
    PubCmd = application:get_env(?APP, pub_cmd),
    #{pub_cmd => PubCmd,
      timeout => Timeout,
      type => Type,
      pool => ?APP}.
