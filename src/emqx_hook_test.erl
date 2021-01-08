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

-module(emqx_hook_test).

-include_lib("emqx/include/emqx.hrl").

-export([ load/1
          , unload/0
        ]).

%% Client Lifecircle Hooks
-export([
         on_client_connected/3
        ]).

%% Session Lifecircle Hooks
-export([
         on_session_subscribed/4
         , on_session_unsubscribed/4
         , on_session_terminated/4
        ]).

%% Called when the plugin application start
load(Env) ->
    emqx:hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
    emqx:hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
    emqx:hook('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
    emqx:hook('session.terminated',  {?MODULE, on_session_terminated, [Env]}).

%%--------------------------------------------------------------------
%% Client Lifecircle Hooks
%%--------------------------------------------------------------------
%%

%% Example payload
%Client(web_521_1234567891) connected, ClientInfo:
%#{anonymous => true,auth_result => success,
%   clientid => <<"web_521_1234567891">>,is_bridge => false,
%   is_superuser => false,mountpoint => undefined,
%   peerhost => {127,0,0,1},
%   protocol => mqtt,sockport => 1883,username => <<"521">>,zone => external}
%, ConnInfo:
%#{clean_start => true,clientid => <<"web_521_1234567891">>,
%   conn_mod => emqx_connection,conn_props => #{},connected_at => 1610096741534,
%   expiry_interval => 0,keepalive => 60,peercert => nossl,
%   peername => {{127,0,0,1},54186},
%   proto_name => <<"MQTT">>,proto_ver => 4,receive_maximum => 32,
%   sockname => {{127,0,0,1},1883},
%   socktype => tcp,username => <<"521">>}
%%
on_client_connected(#{clientid := ClientID, username := UserName}, ConnInfo = #{connected_at := ConnectedAt}, Env) ->
    Payload = jsone:encode(#{event => on_client_connected, 
                             clientid => ClientID, 
                             username => UserName, 
                             created_at => ConnectedAt}),
    publish_broker(Payload, Env).

%%--------------------------------------------------------------------
%% Session Lifecircle Hooks
%%--------------------------------------------------------------------

on_session_subscribed(#{clientid := ClientID, username := UserName}, Topic, SubOpts, Env) ->
    Payload = jsone:encode(#{event => on_session_subscribed, 
                             clientid => ClientID, 
                             username => UserName, 
                             topic => Topic, 
                             created_at => os:system_time(millisecond)}),
    publish_broker(Payload, Env).

on_session_unsubscribed(#{clientid := ClientID, username := UserName}, Topic, Opts, Env) ->
    Payload = jsone:encode(#{event => on_session_unsubscribed, 
                             clientid => ClientID, 
                             username => UserName, 
                             topic => Topic, 
                             created_at => os:system_time(millisecond)}),
    publish_broker(Payload, Env).

on_session_terminated(#{clientid := ClientID, username := UserName}, Reason, SessInfo, Env) ->
    Payload = jsone:encode(#{event => on_session_terminated, 
                             clientid => ClientID, 
                             username => UserName, 
                             created_at => os:system_time(millisecond)}),
    publish_broker(Payload, Env).

publish_broker(Payload, #{pub_cmd := PubCmd, timeout := Timeout, type := Type, pool := Pool}) ->
    case emqx_hook_test_cli:q(Pool, Type, PubCmd, Payload, Timeout) of
        {ok, [_]} -> ok;
        {error, Reason} ->
            ?LOG(error, "[Redis] do_check_acl error: ~p", [Reason]),
            ok
    end.


%% Called when the plugin application stop
unload() ->
    emqx:unhook('client.connected',    {?MODULE, on_client_connected}),
    emqx:unhook('session.subscribed',  {?MODULE, on_session_subscribed}),
    emqx:unhook('session.unsubscribed',{?MODULE, on_session_unsubscribed}),
    emqx:unhook('session.terminated',  {?MODULE, on_session_terminated}).
