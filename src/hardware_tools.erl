-module(hardware_tools).

-export([cleanup/0, mode_bcm/0, setup_pin/2, pin_output/2]).

-include_lib("kernel/include/logger.hrl").

-define(PY_SERVER, application:get_env(basement_core, py_server, py@localhost)).

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
