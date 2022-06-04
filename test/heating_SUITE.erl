-module(heating_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("../src/heating.hrl").

all() ->
    [{group, basic}].

groups() ->
    [{basic, [parallel], basic_cases()}].

basic_cases() ->
    [can_read_temperature,
     can_run_circuts_one_the_time,
     circuts_are_blocked_and_unblocked,
     run_circut_with_index].

init_per_suite(Config) ->
    meck_hardware_tools(),
    meck_erlcron(),
    Config.

end_per_suite(_Config) ->
    meck:unload(erlcron),
    meck:unload(hardware_api).

init_per_group(basic, Config) ->
    start_heating_server(Config).

end_per_group(basic, Config) ->
    ?config(heating_server_task, Config) ! stop,
    Config.

can_read_temperature(_Config) ->
    Pid = spawn_heating_server(),
    {ok, Temps} = gen_server:call(Pid, get_temps),
    ?assertEqual([{high, null}, {low, null}, {floor_heating, null}], Temps),
    Pid ! {check_temperature, all},
    {ok, Temps2} = gen_server:call(Pid, get_temps),
    ?assert(lists:all(fun({_, Temp}) -> is_float(Temp) end, Temps2)).

can_run_circuts_one_the_time(_Config) ->
    Pid = spawn_heating_server(),
    ok = gen_server:cast(Pid, {run_circut, high}),
    {ok, #state{pomp = #cwu_pomp{status = true}, circuts = Circuts}} =
        gen_server:call(Pid, get_config),
    [#circut{status = running}, #circut{status = idle}, #circut{status = idle}] = Circuts,
    gen_server:cast(Pid, {run_circut, low}),
    {ok, #state{pomp = #cwu_pomp{status = true}, circuts = Circuts2}} =
        gen_server:call(Pid, get_config),
    [#circut{status = running}, #circut{status = idle}, #circut{status = idle}] = Circuts2,
    % Stop circut
    Pid ! {stop_circut, high},
    gen_server:cast(Pid, {run_circut, low}),
    {ok, #state{pomp = #cwu_pomp{status = true}, circuts = Circuts3}} =
        gen_server:call(Pid, get_config),
    [#circut{status = blocked}, #circut{status = running}, #circut{status = idle}] = Circuts3,
    % Can run floor
    gen_server:cast(Pid, {run_circut, floor_heating}),
    {ok, #state{circuts = Circuts4}} = gen_server:call(Pid, get_config),
    [_, #circut{status = running}, #circut{status = running}] = Circuts4.

circuts_are_blocked_and_unblocked(_Config) ->
    Pid = spawn_heating_server(),
    {ok, #state{circuts = [High | Tail]} = State} = gen_server:call(Pid, get_config),
    High2 = High#circut{break_duration = {0, 0, 1}, running_duration = {0, 0, 1}},
    gen_server:cast(Pid, {set_config, State#state{circuts = [High2 | Tail]}}),

    ok = gen_server:cast(Pid, {run_circut, high}),
    timer:sleep(500),
    {ok, #state{pomp = #cwu_pomp{status = true}, circuts = [#circut{status = running} | _]}} =
        gen_server:call(Pid, get_config),
    timer:sleep(1200),
    {ok,
     #state{pomp = #cwu_pomp{status = false}, circuts = [#circut{status = blocked} | _]}} =
        gen_server:call(Pid, get_config),
    timer:sleep(500),
    {ok, #state{circuts = [#circut{status = idle} | _]}} = gen_server:call(Pid, get_config).

run_circut_with_index(_Config) ->
    Pid = spawn_heating_server(),
    ok = gen_server:cast(Pid, {run_circut, 0}),
    ok = gen_server:cast(Pid, {run_circut, 1}),
    ok = gen_server:cast(Pid, {run_circut, 2}),
    ok = gen_server:cast(Pid, {run_circut, 3}),
    ok = gen_server:cast(Pid, {run_circut, 4}),
    {ok,
     #state{pomp = #cwu_pomp{status = true},
            circuts =
                [#circut{status = running}, #circut{status = idle}, #circut{status = running}]}} =
        gen_server:call(Pid, get_config).

%% Internal

spawn_heating_server() ->
    {ok, Pid} = gen_server:start_link(heating_server, [], []),
    Pid.

rand_temp() ->
    float(rand:uniform(40) + 10).

start_heating_server(Config) ->
    Pid = spawn(fun() ->
                   {ok, _} = heating_server:start_link(),
                   receive stop -> ok end
                end),
    [{heating_server_task, Pid} | Config].

meck_erlcron() ->
    meck:new(erlcron, [no_link, non_strict]),
    meck:expect(erlcron, get_all_jobs, fun() -> [] end),
    meck:expect(erlcron, daily, fun(_, _) -> ok end),
    meck:expect(erlcron, weekly, fun(_, _) -> ok end).

meck_hardware_tools() ->
    meck:new(hardware_api, [no_link]),
    meck:expect(hardware_api,
                write_pins,
                fun ([23, 18], high) ->
                        ok;
                    ([23, 18], low) ->
                        ok;
                    ([24, 18], high) ->
                        ok;
                    ([24, 18], low) ->
                        ok;
                    ([25], high) ->
                        ok;
                    ([25], low) ->
                        ok
                end),
    meck:expect(hardware_api,
                read_temperature_all,
                fun() ->
                   [{"3c01f095d8f4", rand_temp()},
                    {"01183362faff", rand_temp()},
                    {"011833561aff", rand_temp()}]
                end).

default_state() ->
    C1 = #circut{name = high,
                 valve_pin = 23,
                 running_duration = {0, 0, 10},
                 break_duration = {0, 0, 5},
                 planned_runs = [],
                 max_temp = 39.0,
                 min_temp = 36.0,
                 thermometer_id = "011833561aff",
                 status = idle},
    C2 = #circut{name = low,
                 valve_pin = 24,
                 running_duration = {0, 0, 10},
                 break_duration = {0, 0, 5},
                 max_temp = 40.0,
                 min_temp = 35.0,
                 thermometer_id = "01183362faff",
                 status = idle},
    C3 = #circut{name = floor_heating,
                 valve_pin = 25,
                 running_duration = {1, 0, 0},
                 break_duration = {0, 1, 0},
                 max_temp = 24.5,
                 min_temp = 23.0,
                 thermometer_id = "3c01f095d8f4",
                 status = idle},

    #state{circuts = [C1, C2, C3],
           pomp = #cwu_pomp{pin = 18, status = false},
           temp_read_interval = {0, 0, 1},
           boiler_thermometer_id = "",
           boiler_min_temp = 40.0,
           boiler_temp = null}.
