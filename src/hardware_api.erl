-module(hardware_api).

%% Thermometers API
-export([scan_w1_devices/0, scan_w1_thermometers/0, read_temperature/1,
         read_temperature_all/0]).
%% GPIO API
-export([read_pin/1, read_pins/1, write_pin/2, write_pins/2]).

-define(GPIO, "/sys/class/gpio").
-define(W1, "/sys/bus/w1/devices").

%% Thermometers API

-spec scan_w1_devices() -> [string()].
scan_w1_devices() ->
    {ok, D} = file:list_dir(?W1),
    D.

-spec scan_w1_thermometers() -> [string()].
scan_w1_thermometers() ->
    lists:filtermap(fun is_thermometer/1, scan_w1_devices()).

-spec read_temperature(string()) -> {ok, float()} | {error, cannot_read | cannot_parse}.
read_temperature(Name) ->
    Name2 = lists:flatten([?W1, "/28-", Name, "/temperature"]),
    case file:read_file(Name2) of
        {ok, Temp} ->
            case string:to_integer(binary_to_list(Temp)) of
                {error, _} ->
                    {error, cannot_parse};
                {TempInt, _} ->
                    {ok, TempInt / 1000}
            end;
        _ ->
            {error, cannot_read}
    end.

-spec read_temperature_all() -> [{string(), float()}].
read_temperature_all() ->
    lists:filtermap(fun(T) ->
                       case read_temperature(T) of
                           {ok, Temp} -> {true, {T, Temp}};
                           _ -> false
                       end
                    end,
                    scan_w1_thermometers()).

%% GPIO API

-spec read_pin(integer()) -> high | low | undefined.
read_pin(Pin) ->
    case file:read_file(value_path(Pin)) of
        {ok, <<"0\n">>} ->
            low;
        {ok, <<"1\n">>} ->
            high;
        _E ->
            undefined
    end.

-spec read_pins([integer()]) -> [{integer(), high | low | undefined}].
read_pins(Pins) ->
    [{P, read_pin(P)} || P <- Pins].

-spec write_pin(integer(), high | low) -> ok | {error, term()}.
write_pin(Pin, Value) ->
    file:write_file(value_path(Pin), value_to_bin(Value)).

-spec write_pins([integer()], high | low) -> [ok | {error, term()}].
write_pins(Pins, Value) ->
    [write_pin(P, Value) || P <- Pins].

%% Internal

-spec value_path(integer()) -> iolist().
value_path(Pin) ->
    [?GPIO, "/gpio", integer_to_list(Pin), "/value"].

value_to_bin(high) ->
    <<"1">>;
value_to_bin(low) ->
    <<"0">>.

is_thermometer([$2, $8, $- | Name]) ->
    {true, Name};
is_thermometer(_) ->
    false.
