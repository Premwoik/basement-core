%% @doc This is a gen_server that takes care of the water circulation. It ensures to keep water warm in the pipes at the scheduled hours
%% Server working modes:
%% - running manualy for a default interval (can be shorter when temp reaches max value)
%% - at the scheduled hours temp is hold between the max/min range based on the thermostat attatched to the pipe
%% - each run have maximum running time to prevent overheat the mechanic facilities (should be less than 10 mins)
%% - after each run there is a breakdown to decreased the ficilities temperature (should be 5 min or sth like that)
%% - during breakdown the circut cannot be run in any way
-module(heating_server).

-behaviour(gen_server).

-export([start_link/0, run_circut/1, get_temps/0, set_auto/2, init/1, handle_call/3,
         handle_info/2, handle_cast/2]).

-include_lib("kernel/include/logger.hrl").

-type circut_status() :: idle | running | blocked.
-type planned_run() :: {StartTime :: calendar:time(), Duration :: calendar:time()}.
-type millis() :: integer().

-record(circut,
        {name :: atom(),
         interval :: calendar:time(),
         max_temp :: float(),
         min_temp :: float(),
         status :: circut_status(),
         thermometer_id :: string(),
         auto_allow = false :: boolean(),
         planned_runs = [] :: [planned_run()],
         current_temp = null :: float() | null}).
-record(state,
        {circuts :: [#circut{}], min_boiler_temp :: float(), boiler_temp :: float()}).

-define(TEMP_INTERVAL, 30000).
-define(CIRCUT_RUNNING_TIME, 50000).
-define(CIRCUT_BLOCKING_TIME, 50000).
-define(TEMP_SERVER, {thermostats_server, 'py@192.168.2.142'}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec run_circut(CircutName :: atom()) -> ok.
run_circut(Circut) ->
    gen_server:cast(?MODULE, {run_circut, Circut}).

-spec get_temps() -> {CircutName :: atom(), TempValue :: float()}.
get_temps() ->
    {ok, Temps} = gen_server:call(?MODULE, get_temps),
    Temps.

set_auto(Name, Value) ->
    gen_server:cast(?MODULE, {set_auto, Name, Value}).

init(_Args) ->
    logger:info("Heating server started!"),
    C1 = #circut{name = low,
                 interval = {0, 10, 0},
                 max_temp = 40.0,
                 min_temp = 30.0,
                 thermometer_id = "03172125f1ff",
                 status = idle},
    C2 = #circut{name = high,
                 interval = {0, 10, 0},
                 max_temp = 40.0,
                 min_temp = 30.0,
                 thermometer_id = "",
                 status = idle},

    timer_check_temperature(self()),
    Circuts = [C1, C2],
    init_runs_timers(self(), Circuts),

    {ok, #state{circuts = [C1, C2], boiler_temp = 0}}.

handle_cast({set_auto, Name, Value}, State) ->
    State2 = update_circut(State, Name, fun(Circut) -> Circut#circut{auto_allow = Value} end),
    {noreply, State2};
handle_cast({run_circut, Name}, State) ->
    State2 =
        update_circut(State,
                      Name,
                      fun(Circut) ->
                         case Circut#circut.status of
                             idle ->
                                 logger:info("Starting pomp on circut ~p", [Name]),
                                 send_pomp_start_signal(Circut),
                                 timer_stop_circut(self(), Circut),
                                 Circut#circut{status = running};
                             blocked ->
                                 logger:info("Circut ~p cannot be run as frequent. Wait some time.",
                                             [Name]),
                                 Circut;
                             running ->
                                 logger:info("Circut ~p is just running", [Name]),
                                 Circut
                         end
                      end),
    {noreply, State2};
handle_cast(_, State) ->
    {noreply, State}.

handle_call(get_temps, _From, State) ->
    Res = lists:map(fun(C) -> {C#circut.name, C#circut.current_temp} end,
                    State#state.circuts),
    {reply, {ok, Res}, State};
handle_call(_, _From, State) ->
    {reply, {error, unsupported_cmd}, State}.

handle_info({unblock_circut, Name}, State) ->
    State2 =
        update_circut(State,
                      Name,
                      fun(Circut) ->
                         case Circut#circut.status of
                             blocked ->
                                 logger:info("Unblocking circut ~p", [Name]),
                                 Circut#circut{status = idle};
                             _ ->
                                 logger:info("Circut was not blocked ~p", [Name]),
                                 Circut
                         end
                      end),
    {noreply, State2};
handle_info({stop_circut, Name}, State) ->
    State2 =
        update_circut(State,
                      Name,
                      fun(Circut) ->
                         ?LOG_INFO("Stopping pomp on circut ~p", [Name]),
                         send_pomp_stop_signal(Circut),
                         timer_unblock_circut(self(), Circut),
                         Circut#circut{status = blocked}
                      end),
    {noreply, State2};
handle_info({check_temperature, all}, State) ->
    timer_check_temperature(self()),
    Temps = send_get_temps_signal(),
    State2 =
        update_circuts(State,
                       fun(Circut) ->
                          NewTemp = proplists:get_value(Circut#circut.thermometer_id, Temps, null),
                          ok = stop_circut_when_temp_max(self(), NewTemp, Circut),
                          ok = run_circut_when_temp_min(self(), NewTemp, Circut),
                          Circut#circut{current_temp = NewTemp}
                       end),
    {noreply, State2};
handle_info({open_running_window, Name}, State) ->
    ok = run_circut(Name),
    State2 = update_circut(State, Name, fun(Circut) -> Circut#circut{auto_allow = true} end),
    {noreply, State2};
handle_info({close_running_window, Name, {Time, MDuration}}, State) ->
    ok = timer_allow_auto(self(), Name, Time, MDuration),
    State2 = update_circut(State, Name, fun(Circut) -> Circut#circut{auto_allow = false} end),
    {noreply, State2};
handle_info(_, State) ->
    {noreply, State}.

% Internal
%
%

init_runs_timers(Pid, Circuts) when is_list(Circuts) ->
    lists:map(fun(C) -> init_runs_timers(Pid, C) end, Circuts);
init_runs_timers(Pid, Circut) ->
    lists:map(fun({Time, Duration}) ->
                 MDuration = interval_to_milils(Duration),
                 timer_allow_auto(Pid, Circut#circut.name, Time, MDuration)
              end,
              Circut#circut.planned_runs).

-spec time_difference(calendar:time(), calendar:time()) -> millis().
time_difference(Time1, Time2) ->
    abs(calendar:time_to_seconds(Time1) - calendar:time_to_seconds(Time2)) * 1000.

-spec interval_to_milils(calendar:time()) -> millis().
interval_to_milils(Time) ->
    calendar:time_to_seconds(Time) * 1000.

-spec stop_circut_when_temp_max(pid(), float(), #circut{}) -> ok.
stop_circut_when_temp_max(_Pid, null, _) ->
    ok;
stop_circut_when_temp_max(Pid,
                          Temp,
                          #circut{name = Name,
                                  max_temp = MaxTemp,
                                  status = running,
                                  auto_allow = true}) ->
    case Temp >= MaxTemp of
        true ->
            Pid ! {stop_circut, Name};
        false ->
            ok
    end,
    ok;
stop_circut_when_temp_max(_Pid, _, _) ->
    ok.

-spec run_circut_when_temp_min(pid(), float(), #circut{}) -> ok.
run_circut_when_temp_min(_Pid, null, _) ->
    ok;
run_circut_when_temp_min(Pid,
                         Temp,
                         #circut{name = Name,
                                 min_temp = MinTemp,
                                 status = idle,
                                 auto_allow = true}) ->
    case Temp < MinTemp of
        true ->
            gen_server:cast(Pid, {run_circut, Name});
        false ->
            ok
    end,
    ok;
run_circut_when_temp_min(_Pid, _, _) ->
    ok.

-spec send_get_temps_signal() -> [{Addr :: string(), Temp :: float()}].
send_get_temps_signal() ->
    try gen_server:call(?TEMP_SERVER, <<"get_temps">>) of
        {ok, Temps} ->
            ?LOG_DEBUG("Read temperatures ~p", [Temps]),
            Temps
    catch
        _Error ->
            ?LOG_ERROR("Error occured when tried to read temperatures"),
            []
    end.

send_pomp_start_signal(Circut) ->
    logger:info("Starting pomp ~p", [Circut#circut.name]).

send_pomp_stop_signal(Circut) ->
    logger:info("Stopping pomp ~p", [Circut#circut.name]).

timer_check_temperature(Pid) ->
    erlang:send_after(?TEMP_INTERVAL, Pid, {check_temperature, all}).

timer_stop_circut(Pid, Circut) ->
    erlang:send_after(?CIRCUT_RUNNING_TIME, Pid, {stop_circut, Circut#circut.name}).

timer_unblock_circut(Pid, Circut) ->
    erlang:send_after(?CIRCUT_BLOCKING_TIME, Pid, {unblock_circut, Circut#circut.name}).

timer_allow_auto(Pid, Name, Time, MDuration) ->
    MTime = time_difference(time(), Time),
    timer:send_after(MTime, Pid, {open_running_window, Name}),
    timer:send_after(MTime + MDuration, Pid, {close_running_window, Name, {Time, MDuration}}),
    ok.

-spec update_circut(#state{}, atom(), function()) -> #state{}.
update_circut(#state{circuts = Circuts} = State, Name, Fun) ->
    Circuts2 =
        lists:map(fun(C) ->
                     case C#circut.name == Name of
                         true -> Fun(C);
                         _ -> C
                     end
                  end,
                  Circuts),
    State#state{circuts = Circuts2}.

-spec update_circuts(#state{}, function()) -> #state{}.
update_circuts(#state{circuts = Circuts} = State, Fun) ->
    Circuts2 = lists:map(fun(C) -> Fun(C) end, Circuts),
    State#state{circuts = Circuts2}.
