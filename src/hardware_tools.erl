-module(hardware_tools).

% GPIO
-export([cleanup/0, mode_bcm/0, setup_pin/2, pin_output/2]).

% Thermometers 1Wire
-export([read_all_thermostats/0]).

-include_lib("kernel/include/logger.hrl").

-define(PY_SERVER, application:get_env(basement_core, py_server, py@localhost)).
-define(THERMOSTATS_SERVER, {thermostats_server, ?PY_SERVER}).

-ignore_xref([cleanup/0, mode_bcm/0, setup_pin/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec cleanup() -> ok.
cleanup() ->
    call(cleanup, []).

-spec mode_bcm() -> ok.
mode_bcm() ->
    call(set_mode_bcm, []).

-spec setup_pin(tuple(), input | output) -> ok.
setup_pin(Pin, input) ->
    call(setup_pin_in, [Pin]);
setup_pin(Pin, output) ->
    call(setup_pin_out, [Pin]).

-spec pin_output(tuple(), high | low) -> ok.
pin_output(Pin, high) ->
    call(pin_output_high, [Pin]);
pin_output(Pin, low) ->
    call(pin_output_low, [Pin]).

-spec read_all_thermostats() -> [{Addr :: string(), Temp :: float()}].
read_all_thermostats() ->
    try gen_server:call(?THERMOSTATS_SERVER, <<"get_temps">>) of
        {ok, Temps} ->
            ?LOG_INFO("Read temperatures ~p", [Temps]),
            Temps;
        {error, Error} ->
            ?LOG_ERROR("Read temperatures ERROR - ~p", [Error]),
            []
    catch
        exit:_ ->
            ?LOG_ERROR("Cannot connect to python node to read temperatures"),
            [];
        _Error ->
            ?LOG_ERROR("Unexpected ERROR occured when tried to read temperatures"),
            []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec call(atom(), list()) -> any().
call(Fun, Args) ->
    try rpc:call(?PY_SERVER, gpio_tools, Fun, Args) of
        Res ->
            Res
    catch
        exit:_ ->
            ?LOG_ERROR("Cannot connect to python node"),
            {error, cannot_connect};
        _Error ->
            ?LOG_ERROR("Unexpected ERROR occured when tried to use GPIO"),
            {error, unknown}
    end.
