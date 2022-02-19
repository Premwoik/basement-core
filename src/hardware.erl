-module(hardware).

%% Thermometers API
-export([scan_w1_devices/0, scan_w1_thermometers/0, read_temperature/1]).
%% GPIO API
-export([read_pin/1, write_pin/2]).

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

-spec read_temperature(string()) -> {ok, float()} | {error, cannot_read}.
read_temperature(Name) ->
    Name2 = lists:flatten([?W1, "/28-", Name, "/temperature"]),
    case file:read_file(Name2) of
        {ok, Temp} ->
            Temp2 = binary:part(Temp, {byte_size(Temp), -1}),
            {ok, binary_to_integer(Temp2) / 1000};
        _ ->
            {error, cannot_read}
    end.

%% GPIO API

-spec read_pin(integer()) -> high | low | undefined.
read_pin(Pin) ->
    case file:read_file(value_path(Pin)) of
        {ok, <<"0">>} ->
            low;
        {ok, <<"1">>} ->
            high;
        _ ->
            undefined
    end.

-spec write_pin(integer(), high | low) -> ok | {error, term()}.
write_pin(Pin, Value) ->
    file:write_file(value_path(Pin), value_to_bin(Value)).

%% Internal

-spec value_path(integer()) -> iolist().
value_path(Pin) ->
    [?GPIO, "/gpio", integer_to_list(Pin), "value"].

value_to_bin(high) ->
    <<"1">>;
value_to_bin(low) ->
    <<"0">>.

is_thermometer([$2, $8, $- | Name]) ->
    {true, Name};
is_thermometer(_) ->
    false.
