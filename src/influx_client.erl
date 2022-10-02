-module(influx_client).

-export([write_temperatures/1]).

-include_lib("kernel/include/logger.hrl").

-define(URL, <<"http://192.168.2.100:8086">>).
-define(ORG, <<"SmartHome">>).
-define(BUCKET, <<"default">>).
-define(TOKEN,
        <<"Bearer fUqLk2fqqb62jyE2PYNpx1mNbu38s75SKN7thO1nKpNqf2vRzb24QWopAlUjh"
          "-WM54xJ2KJA2_jXDYzGSlPKDQ==">>).

-spec write_temperatures([{Thermometer, Temperature}]) -> ok | error
    when Thermometer :: string(),
         Temperature :: float().
write_temperatures(Pairs) ->
    F = fun({Thermometer, Temperature}) ->
           io_lib:format("water_temperature_sensor,thermometer_id=~s temperature=~f",
                         [Thermometer, Temperature])
        end,
    Payload = iolist_to_binary(lists:join("\n", lists:map(F, Pairs))),
    write_payload(Payload).

write_payload(Payload) ->
    URL = hackney_url:make_url(?URL, <<"api/v2/write">>, [{org, ?ORG}, {bucket, ?BUCKET}]),
    Headers = [{<<"Content-Type">>, <<"text/plain">>}, {"Authorization", ?TOKEN}],
    case hackney:request(post, URL, Headers, Payload, []) of
        {ok, 204, _, _} ->
            ok;
        {ok, Code, _, _} ->
            ?LOG_WARNING("InfluxDB :: ~B :: Cannot write payload: ~n------PAYLOOAD------~n~s~n-------------------", [Code, Payload]),
            error
    end.
