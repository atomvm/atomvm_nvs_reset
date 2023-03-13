%%
%% Copyright (c) 2021 dushin.net
%% All rights reserved.
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
-module(nvs_reset_example).

-export([start/0]).

-define(RESET_PIN, 0).

start() ->

    io:format("NVS test value at startup: ~p~n", [esp:nvs_get_binary(?MODULE, test)]),

    TestValue = <<"this is a test">>,
    io:format("Set NVS test value to: ~p~n", [TestValue]),
    esp:nvs_set_binary(?MODULE, test, TestValue),

    Config = #{
        reset_pin => ?RESET_PIN
    },
    {ok, _Pid} = atomvm_nvs_reset:start_link(Config),
    io:format("NVS Reset started on pin ~p.~n", [?RESET_PIN]),

    timer:sleep(infinity).
