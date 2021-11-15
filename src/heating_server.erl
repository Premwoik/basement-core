%% @doc This is a gen_server that takes care of the water circulation. It ensures to keep water warm in the pipes at the scheduled hours
%% This module use a Pyrlang server to read the temperatures :)
%%
%% Server working details:
%% - running manualy for a default interval (can be shorter when temp reaches max value)
%% - at the scheduled hours temp is hold between the max/min range based on the thermostat attatched to the pipe
%% - each run have maximum running time to prevent overheat the mechanic facilities (should be less than 10 mins)
%% - after each run there is a breakdown to decreased the ficilities temperature (should be 5 min or sth like that)
%% - during breakdown the circut cannot be run in any way
%% Done:
%% - add ref for a timers and cancel stopping timer when temp reach desired target erlier.
%% - add boiler temp guard - when temp on boiler is too low, don't allow to start pomp
%% What is not done yet?
%% - TODO maybe add unit tests?
%%
-module(heating_server).

-behaviour(gen_server).

-export([start_link/0, run_circut/1, get_temps/0, set_auto/2, init/1, handle_call/3,
         handle_info/2, handle_cast/2]).

-include_lib("kernel/include/logger.hrl").

-type day() :: erlcron:dow() | null.
-type circut_status() :: idle | running | blocked.
-type planned_run() ::
    {Day :: day(), StartTime :: calendar:time(), Duration :: calendar:time()}.
-type millis() :: integer().

-record(circut,
        {name :: atom(),
         break_duration :: calendar:time(),
         running_duration :: calendar:time(),
         stop_timer_ref = null :: reference() | null,
         max_temp :: float(),
         min_temp :: float(),
         status :: circut_status(),
         valve_pin :: integer(),
         thermometer_id :: string(),
         auto_allow = false :: boolean(),
         planned_runs = [] :: [planned_run()],
         current_temp = null :: float() | null}).
-record(state,
        {circuts :: [#circut{}],
         temp_read_interval :: calendar:time(),
         pomp_pin :: integer(),
         min_boiler_temp :: float(),
         boiler_thermometer_id :: string(),
         boiler_min_temp :: float(),
         boiler_temp :: float() | null}).

-define(TEMP_SERVER,
        {thermostats_server, application:get_env(basement_core, py_server, py@localhost)}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts a server.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Runs the circut under given name.
-spec run_circut(CircutName :: atom()) -> ok.
run_circut(Circut) ->
    gen_server:cast(?MODULE, {run_circut, Circut}).

%% @doc Gets the temp of the water in circut.
-spec get_temps() -> {CircutName :: atom(), TempValue :: float()}.
get_temps() ->
    {ok, Temps} = gen_server:call(?MODULE, get_temps),
    Temps.

%% @doc Allows to set auto mode manually (circut can be run based on the temp on the pipe).
-spec set_auto(CircutName :: string(), Value :: boolean()) -> ok.
set_auto(Name, Value) ->
    gen_server:cast(?MODULE, {set_auto, Name, Value}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    logger:info("Heating server started!"),
    C1 = #circut{name = high,
                 valve_pin = 23,
                 running_duration = {0, 10, 0},
                 break_duration = {0, 5, 0},
                 planned_runs = [{null, {6, 20, 0}, {1, 0, 0}}, {null, {20, 0, 0}, {2, 0, 0}}],
                 max_temp = 40.0,
                 min_temp = 35.0,
                 thermometer_id = "011833561aff",
                 status = idle},
    C2 = #circut{name = low,
                 valve_pin = 24,
                 running_duration = {0, 10, 0},
                 break_duration = {0, 5, 0},
                 max_temp = 40.0,
                 min_temp = 35.0,
                 thermometer_id = "01183362faff",
                 status = idle},

    Circuts = [C1, C2],

    State =
        #state{circuts = Circuts,
               pomp_pin = 18,
               temp_read_interval = {0, 0, 10},
               boiler_thermometer_id = '',
               boiler_min_temp = 40.0,
               boiler_temp = null},

    timer_check_temperature(self(), State#state.temp_read_interval),
    init_runs_timers(self(), Circuts),

    {ok, State}.

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
                                 case State#state.boiler_temp of
                                     Temp
                                         when (Temp == null)
                                              or (Temp >= State#state.boiler_min_temp) ->
                                         logger:info("Starting pomp on circut ~p", [Name]),
                                         send_pomp_start_signal(State, Circut),
                                         Ref = timer_stop_circut(self(), Circut),
                                         Circut#circut{status = running, stop_timer_ref = Ref};
                                     _ ->
                                         logger:info("Boiler temp is too low ~p", [Name]),
                                         Circut
                                 end;
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
                         cancel_timer(Circut#circut.stop_timer_ref),
                         send_pomp_stop_signal(State, Circut),
                         timer_unblock_circut(self(), Circut),
                         Circut#circut{status = blocked}
                      end),
    {noreply, State2};
handle_info({check_temperature, all}, State) ->
    timer_check_temperature(self(), State#state.temp_read_interval),
    Temps = send_get_temps_signal(),
    State2 = update_boiler_temp(State, Temps),
    State3 =
        update_circuts(State2,
                       fun(Circut) ->
                          NewTemp = proplists:get_value(Circut#circut.thermometer_id, Temps, null),
                          ok = stop_circut_when_temp_max(self(), NewTemp, Circut),
                          ok = run_circut_when_temp_min(self(), NewTemp, Circut),
                          Circut#circut{current_temp = NewTemp}
                       end),
    {noreply, State3};
handle_info({open_running_window, Name}, State) ->
    ?LOG_INFO("Open auto running window ~p", [Name]),
    %ok = gen_server:cast(self(), {run_circut, Name}),
    State2 = update_circut(State, Name, fun(Circut) -> Circut#circut{auto_allow = true} end),
    {noreply, State2};
handle_info({close_running_window, Name}, State) ->
    ?LOG_INFO("Close auto running window ~p", [Name]),
    State2 = update_circut(State, Name, fun(Circut) -> Circut#circut{auto_allow = false} end),
    {noreply, State2};
handle_info(_, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cancel_timer(null) ->
    ok;
cancel_timer(Ref) ->
    timer:cancel(Ref).

init_runs_timers(Pid, Circuts) when is_list(Circuts) ->
    lists:map(fun(C) -> init_runs_timers(Pid, C) end, Circuts);
init_runs_timers(Pid, Circut) ->
    lists:map(fun ({null, Time, Duration}) ->
                      ?LOG_INFO("Planned daily auto window at ~p duration ~p", [Time, Duration]),
                      erlcron:daily(Time,
                                    fun() -> cron_allow_auto(Pid, Circut#circut.name, Duration)
                                    end);
                  ({Day, Time, Duration}) ->
                      ?LOG_INFO("Planned weekly auto window on days ~p at ~p duration ~p",
                                [Day, Time, Duration]),
                      erlcron:weekly(Day,
                                     Time,
                                     fun() -> cron_allow_auto(Pid, Circut#circut.name, Duration)
                                     end)
              end,
              Circut#circut.planned_runs).

cron_allow_auto(Pid, Name, Duration) ->
    Pid ! {open_running_window, Name},
    MDuration = interval_to_milils(Duration),
    {ok, _TRef} = timer:send_after(MDuration, Pid, {close_running_window, Name}),
    ok.

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
                                  status = running}) ->
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

send_pomp_start_signal(#state{pomp_pin = PompPin},
                       #circut{name = Name, valve_pin = Pin}) ->
    hardware_tools:pin_output({Pin, PompPin}, low),
    logger:debug("Sending starting pomp signal ~p", [Name]).

send_pomp_stop_signal(#state{pomp_pin = PompPin},
                      #circut{name = Name, valve_pin = Pin}) ->
    hardware_tools:pin_output({Pin, PompPin}, high),
    logger:debug("Sending stopping pomp signal ~p", [Name]).

timer_check_temperature(Pid, Interval) ->
    IntervalMillis = interval_to_milils(Interval),
    timer:send_after(IntervalMillis, Pid, {check_temperature, all}).

timer_stop_circut(Pid, #circut{name = Name, running_duration = RunningTime}) ->
    RunningTimeMillis = interval_to_milils(RunningTime),
    {ok, Ref} = timer:send_after(RunningTimeMillis, Pid, {stop_circut, Name}),
    Ref.

timer_unblock_circut(Pid, #circut{name = Name, break_duration = BreakTime}) ->
    BreakTimeMillis = interval_to_milils(BreakTime),
    timer:send_after(BreakTimeMillis, Pid, {unblock_circut, Name}).

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

update_boiler_temp(State, Temps) ->
    NewTemp = proplists:get_value(State#state.boiler_thermometer_id, Temps, null),
    State#state{boiler_temp = NewTemp}.
