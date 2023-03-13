%%
%% Copyright (c) 2023 dushin.net
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
-module(atomvm_nvs_reset).

-export([start_link/1]).

% -behavior(gen_statem).
-export([init/1, idle/3, button_pressed/3, terminate/2]).

-type config() :: #{
    reset_pin => non_neg_integer(),
    reset_invert_pin => boolean(),
    reset_hold_secs => non_neg_integer(),
    reset_hold_timeout_secs => non_neg_integer(),
    restart_on_reset => boolean()
}.

-export_type([config/0]).

% -define(TRACE_ENABLED, true).
-include_lib("atomvm_lib/include/trace.hrl").

-define(DEFAULT_CONFIG, #{
    reset_pin => 0,
    reset_invert_pin => false,
    reset_hold_secs => 5,
    reset_hold_timeout_secs => 15,
    restart_on_reset => true
}).

%%
%% api
%%

-spec start_link(Config :: config()) -> {ok, Pid :: pid()} | {error, Reasons :: term()}.
start_link(Config) ->
    ?TRACE("Starting ~p with config ~p", [?MODULE, Config]),
    gen_statem:start(?MODULE, maps:merge(?DEFAULT_CONFIG, Config), []).

%%
%% gen_server implementation
%%

-record(state, {
    config,
    reset_pin,
    gpio_driver,
    last_pressed = 0
}).

%% @hidden
init(Config) ->

    ?TRACE("Initializing with config ~p", [Config]),

    ResetPin = maps:get(reset_pin, Config),
    gpio:set_pin_mode(ResetPin, input),
    case maps:get(reset_invert_pin, Config, false) of
        true ->
            gpio:set_pin_pull(ResetPin, down);
        _ ->
            gpio:set_pin_pull(ResetPin, up)
    end,

    GPIODriver = gpio:start(),
    gpio:set_int(GPIODriver, ResetPin, both),
    ?TRACE("Started GPIO driver ~p", [GPIODriver]),

    {ok, idle, #state{
        config = Config,
        reset_pin = ResetPin,
        gpio_driver = GPIODriver
    }}.

%% @hidden
idle(info, {gpio_interrupt, ResetPin}, #state{reset_pin = ResetPin } = State) ->
    Config = State#state.config,
    case is_button_pressed(Config) of
        true ->
            io:format("Button ~p pressed.~n", [ResetPin]),
            ResetHoldTimeoutSecs = maps:get(reset_hold_timeout_secs, Config),
            CurrentMs = current_ms(),
            NewState = State#state{last_pressed=CurrentMs},
            Actions = [{state_timeout, ResetHoldTimeoutSecs * 1000, undefined}],
            {next_state, button_pressed, NewState, Actions};
        _ ->
            ?TRACE("Button released in idle state.", []),
            {next_state, idle, State}
    end;
idle(_EventType, _Msg, State) ->
    ?TRACE("spurious message.", []),
    {next_state, idle, State}.

%% @hidden
button_pressed(state_timeout, _Msg, State) ->
    io:format("Timed out waiting for button release.~n", []),
    {next_state, idle, State};
button_pressed(info, {gpio_interrupt, ResetPin}, #state{reset_pin = ResetPin } = State) ->
    Config = State#state.config,
    case is_button_released(Config) of
        true ->
            ?TRACE("Button released in button pressed state.", []),
            CurrentMs = current_ms(),
            ResetHoldSecs = maps:get(reset_hold_secs, Config),
            case (CurrentMs - State#state.last_pressed) > (ResetHoldSecs * 1000) of
                true ->
                    do_nvs_reset(Config);
                _ ->
                    io:format("Button released too early (must be held for ~p secs but only held for ~p ms).~n", [ResetHoldSecs, CurrentMs - State#state.last_pressed]),
                    ok
            end,
            {next_state, idle, State};
        _ ->
            ?TRACE("Button pressed in button_pressed state.", []),
            {next_state, idle, State}
    end;
button_pressed(_EventType, _Msg, State) ->
    ?TRACE("spurious message.", []),
    {next_state, button_pressed, State}.

%% @hidden
terminate(_Reason, State) ->
    Config = State#state.config,
    gpio:remove_int(State#state.gpio_driver, maps:get(reset_pin, Config)),
    % gpio:close(State#state.gpio_driver)
    ok.

%%
%% internal implementation
%%

%% @private
is_button_released(Config) ->
    not is_button_pressed(Config).

%% @private
is_button_pressed(Config) ->
    ResetPin = maps:get(reset_pin, Config),
    Level = gpio:digital_read(ResetPin),
    is_button_pressed(Level, maps:get(reset_invert_pin, Config)).

%% @private
is_button_pressed(low, true) ->
    false;
is_button_pressed(high, true) ->
    true;
is_button_pressed(low, false) ->
    true;
is_button_pressed(high, false) ->
    false.

%% @private
do_nvs_reset(Config) ->
    io:format("Erasing NVS ...~n"),
    esp:nvs_reformat(),
    case maps:get(restart_on_reset, Config, true) of
        true ->
            io:format("Restarting ..."),
            esp:restart();
        _ ->
            ok
    end.

%% @private
current_ms() ->
    erlang:system_time(millisecond).
