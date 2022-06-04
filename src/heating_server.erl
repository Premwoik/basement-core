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
%% @end
-module(heating_server).

-behaviour(gen_server).

-export([start_link/0, register_observer/1, unregister_observer/1,
         is_registered_observer/1, get_config/0, set_config/1, run_circut/1, get_temps/0,
         set_auto/2, init/1, handle_call/3, handle_info/2, handle_cast/2, log_info/0,
         log_error/0]).

-ignore_xref([start_link/0,
              register_observer/1,
              unregister_observer/1,
              get_config/0,
              set_config/1,
              run_circut/1,
              get_temps/0,
              set_auto/2,
              init/1,
              handle_call/3,
              handle_info/2,
              handle_cast/2,
              log_info/0,
              log_error/0]).

-include_lib("kernel/include/logger.hrl").

-include("heating.hrl").

-define(CONFIG_PATH, "heating_state.conf").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts a server.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Runs the circut under given name.
-spec run_circut(CircutName :: atom() | integer()) -> ok.
run_circut(Circut) ->
    gen_server:cast(?MODULE, {run_circut, Circut}).

%% @doc Gets the full state
-spec get_config() -> {ok, #state{}}.
get_config() ->
    gen_server:call(?MODULE, get_config).

%% @doc Set the full state
-spec set_config(#state{}) -> ok.
set_config(Config) ->
    gen_server:cast(?MODULE, {set_config, Config}).

%% @doc Gets the temp of the water in circut.
-spec get_temps() -> {CircutName :: atom(), TempValue :: float()}.
get_temps() ->
    {ok, Temps} = gen_server:call(?MODULE, get_temps),
    Temps.

-spec register_observer(pid()) -> ok.
register_observer(Pid) ->
    gen_server:cast(?MODULE, {register, Pid}).

-spec unregister_observer(pid()) -> ok.
unregister_observer(Pid) ->
    gen_server:cast(?MODULE, {unregister, Pid}).

-spec is_registered_observer(pid()) -> {ok, binary()}.
is_registered_observer(Pid) ->
    gen_server:call(?MODULE, {is_registered, Pid}).

%% @doc Allows to set auto mode manually (circut can be run based on the temp on the pipe).
-spec set_auto(CircutName :: string(), Value :: boolean()) -> ok.
set_auto(Name, Value) ->
    gen_server:cast(?MODULE, {set_auto, Name, Value}).

-spec log_info() -> any().
log_info() ->
    logger:set_module_level(heating_server, info).

-spec log_error() -> any().
log_error() ->
    logger:set_module_level(heating_server, error).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    ?LOG_INFO("Heating server started!"),
    State = try_read_state(),
    timer_check_temperature(self(), State#state.temp_read_interval),
    init_runs_timers(self(), State#state.circuts),
    {ok, State}.

handle_cast({set_auto, Name, Value}, State) ->
    State2 =
        update_circut(State, Name, fun(Circut, _) -> Circut#circut{auto_allow = Value} end),
    {noreply, State2};
handle_cast({run_circut, Id}, State) when is_integer(Id) ->
    case Id > 0 andalso Id =< length(State#state.circuts) of
        true ->
            #circut{name = Name} = lists:nth(Id, State#state.circuts),
            handle_cast({run_circut, Name}, State);
        false ->
            {noreply, State}
    end;
handle_cast({run_circut, Name}, InState) ->
    State2 =
        update_circut(InState,
                      Name,
                      fun(Circut, State) ->
                         case Circut#circut.status of
                             idle -> do_run_circut(Circut, State);
                             blocked ->
                                 ?LOG_INFO("Circut ~p cannot be run as frequent. Wait some time.",
                                           [Name]),
                                 Circut;
                             running ->
                                 ?LOG_INFO("Circut ~p is just running", [Name]),
                                 Circut
                         end
                      end),
    ok = broadcast_status_update(State2),
    {noreply, State2};
handle_cast({register, Pid}, #state{observers = Observers} = State) ->
    ?LOG_INFO("Registered observer ~p", [Pid]),
    State2 = State#state{observers = [Pid | Observers]},
    {noreply, State2};
handle_cast({unregister, Pid}, #state{observers = Observers} = State) ->
    ?LOG_INFO("Unregistered observer ~p", [Pid]),
    Observers2 = lists:filter(fun(P) -> P /= Pid end, Observers),
    State2 = State#state{observers = Observers2},
    {noreply, State2};
handle_cast({set_config, State}, _) ->
    ?LOG_INFO("Setting new config..."),
    lists:foreach(fun erlcron:cancel/1, erlcron:get_all_jobs()),
    init_runs_timers(self(), State#state.circuts),
    save_config(State),
    ?LOG_INFO("Config changed successfully!"),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_call({is_registered, Pid}, _From, #state{observers = Observers} = State) ->
    {reply, {ok, lists:member(Pid, Observers)}, State};
handle_call(get_config, _From, State) ->
    {reply, {ok, State}, State};
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
                      fun(Circut, _) ->
                         case Circut#circut.status of
                             blocked ->
                                 logger:info("Unblocking circut ~p", [Name]),
                                 Circut#circut{status = idle};
                             _ ->
                                 logger:info("Circut was not blocked ~p", [Name]),
                                 Circut
                         end
                      end),

    ok = broadcast_status_update(State2),
    {noreply, State2};
handle_info({stop_circut, Name}, InState) ->
    State2 =
        update_circut(InState,
                      Name,
                      fun (#circut{type = cwu} = Circut, #state{pomp = Pomp} = State) ->
                              ?LOG_INFO("Stopping pomp on circut ~p", [Name]),
                              cancel_timer(Circut#circut.stop_timer_ref),
                              send_stop_signal(State, Circut),
                              timer_unblock_circut(self(), Circut),
                              State2 = State#state{pomp = Pomp#cwu_pomp{status = false}},
                              {Circut#circut{status = blocked}, State2};
                          (#circut{type = floor} = Circut, State) ->
                              ?LOG_INFO("Stopping floor wire on circut ~p", [Name]),
                              cancel_timer(Circut#circut.stop_timer_ref),
                              send_stop_signal(State, Circut),
                              Circut#circut{status = idle}
                      end),
    ok = broadcast_status_update(State2),
    {noreply, State2};
handle_info({check_temperature, all}, State) ->
    timer_check_temperature(self(), State#state.temp_read_interval),
    Temps = hardware_api:read_temperature_all(),
    ?LOG_INFO("Read temperatures: ~p", [Temps]),
    State2 = update_boiler_temp(State, Temps),
    State3 =
        update_circuts(State2,
                       fun(Circut, _) ->
                          NewTemp = proplists:get_value(Circut#circut.thermometer_id, Temps, null),
                          ok = stop_circut_when_temp_max(self(), NewTemp, Circut),
                          ok = run_circut_when_temp_min(self(), NewTemp, Circut),
                          Circut#circut{current_temp = NewTemp}
                       end),
    ok = broadcast_temp_update(State3),
    {noreply, State3};
handle_info({open_running_window, Name}, State) ->
    ?LOG_INFO("Open auto running window ~p", [Name]),
    ok = gen_server:cast(self(), {run_circut, Name}),
    State2 =
        update_circut(State, Name, fun(Circut, _) -> Circut#circut{auto_allow = true} end),
    {noreply, State2};
handle_info({close_running_window, Name}, State) ->
    ?LOG_INFO("Close auto running window ~p", [Name]),
    State2 =
        update_circut(State, Name, fun(Circut, _) -> Circut#circut{auto_allow = false} end),
    {noreply, State2};
handle_info(_, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec do_run_circut(circut(), state()) -> circut().
do_run_circut(#circut{type = cwu, name = Name} = Circut, #state{pomp = Pomp} = State) ->
    case State#state.boiler_temp of
        Temp when (Temp == null) or (Temp >= State#state.boiler_min_temp) ->
            case State#state.pomp of
                %% It is possible to run only one cwu circut at the same time.
                %% Thus when cwu pomp is running the next circut cannot be started.
                #cwu_pomp{status = false} ->
                    ?LOG_INFO("Starting pomp on circut ~p", [Name]),
                    send_start_signal(State, Circut),
                    Ref = timer_stop_circut(self(), Circut),
                    State2 = State#state{pomp = Pomp#cwu_pomp{status = true}},
                    {Circut#circut{status = running, stop_timer_ref = Ref}, State2};
                _ ->
                    ?LOG_INFO("The other C.W.U circut is running"),
                    Circut
            end;
        _ ->
            ?LOG_INFO("Boiler temp is too low ~p", [Name]),
            Circut
    end;
do_run_circut(#circut{type = floor, name = Name} = Circut, State) ->
    send_start_signal(State, Circut),
    Ref = timer_stop_circut(self(), Circut),
    ?LOG_INFO("Starting floor heating on circut ~p", [Name]),
    Circut#circut{status = running, stop_timer_ref = Ref}.

default_state() ->
    C1 = #circut{name = high,
                 valve_pin = 23,
                 running_duration = {0, 10, 0},
                 break_duration = {0, 5, 0},
                 planned_runs = [{null, {6, 20, 0}, {1, 0, 0}}, {null, {8, 0, 0}, {1, 0, 0}}],
                 max_temp = 39.0,
                 min_temp = 36.0,
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
    C3 = #circut{name = floor_heating,
                 valve_pin = 25,
                 running_duration = {1, 0, 0},
                 break_duration = {0, 1, 0},
                 max_temp = 24.5,
                 min_temp = 23.0,
                 thermometer_id = "3c01f095d8f4",
                 status = idle,
                 type = floor},

    #state{circuts = [C1, C2, C3],
           pomp = #cwu_pomp{pin = 18, status = false},
           temp_read_interval = {0, 0, 30},
           boiler_thermometer_id = "",
           boiler_min_temp = 40.0,
           boiler_temp = null}.

try_read_state() ->
    case file:consult(?CONFIG_PATH) of
        {ok, [State]} ->
            clear_state_variables(State);
        _ ->
            State = default_state(),
            save_config(State),
            State
    end.

save_config(State) ->
    ?LOG_INFO("Saving config to file..."),
    file:write_file(?CONFIG_PATH, io_lib:format("~p.~n", [State])).

clear_state_variables(State) ->
    Circuts =
        lists:map(fun(C) ->
                     C#circut{status = idle,
                              current_temp = null,
                              stop_timer_ref = null}
                  end,
                  State#state.circuts),
    State#state{circuts = Circuts, boiler_temp = null}.

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

send_start_signal(_State,
                  #circut{type = floor,
                          name = Name,
                          valve_pin = Pin}) ->
    hardware_api:write_pins([Pin], low),
    logger:debug("Sending starting pomp signal ~p", [Name]);
send_start_signal(#state{pomp = #cwu_pomp{pin = PompPin}},
                  #circut{name = Name, valve_pin = Pin}) ->
    hardware_api:write_pins([Pin, PompPin], low),
    logger:debug("Sending starting pomp signal ~p", [Name]).

send_stop_signal(_State,
                 #circut{type = floor,
                         name = Name,
                         valve_pin = Pin}) ->
    hardware_api:write_pins([Pin], high),
    logger:debug("Sending stopping pomp signal ~p", [Name]);
send_stop_signal(#state{pomp = #cwu_pomp{pin = PompPin}},
                 #circut{name = Name, valve_pin = Pin}) ->
    hardware_api:write_pins([Pin, PompPin], high),
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

-type update_fun() :: fun((circut(), state()) -> {circut(), state()} | circut()).
-type update_acc() :: {state(), [circut()]}.

-spec run_update_fun(circut(), update_acc(), update_fun()) -> update_acc().
run_update_fun(Circut, {State, CirAcc}, Fun) ->
    case Fun(Circut, State) of
        {Circut2, State2} ->
            {State2, [Circut2 | CirAcc]};
        Circut2 ->
            {State, [Circut2 | CirAcc]}
    end.

-spec update_circut(#state{}, atom(), update_fun()) -> #state{}.
update_circut(#state{circuts = Circuts} = InState, Name, Fun) ->
    {State2, Circuts2} =
        lists:foldr(fun(C, {State, CirAcc}) ->
                       case C#circut.name == Name of
                           true -> run_update_fun(C, {State, CirAcc}, Fun);
                           _ -> {State, [C | CirAcc]}
                       end
                    end,
                    {InState, []},
                    Circuts),
    State2#state{circuts = Circuts2}.

-spec update_circuts(#state{}, function()) -> #state{}.
update_circuts(#state{circuts = Circuts} = InState, Fun) ->
    {State2, Circuts2} =
        lists:foldr(fun(Circut, Acc) -> run_update_fun(Circut, Acc, Fun) end,
                    {InState, []},
                    Circuts),
    State2#state{circuts = Circuts2}.

-spec update_boiler_temp(#state{}, [{string(), float()}]) -> #state{}.
update_boiler_temp(State, Temps) ->
    NewTemp = proplists:get_value(State#state.boiler_thermometer_id, Temps, null),
    State#state{boiler_temp = NewTemp}.

-spec broadcast_temp_update(#state{}) -> ok.
broadcast_temp_update(#state{observers = []}) ->
    ok;
broadcast_temp_update(#state{observers = Pids, circuts = Circuts}) ->
    Data =
        lists:map(fun(#circut{current_temp = Temp, name = Name}) -> {Name, Temp} end, Circuts),
    lists:foreach(fun(Pid) -> Pid ! {temp_update, Data} end, Pids),
    ok.

-spec broadcast_status_update(#state{}) -> ok.
broadcast_status_update(#state{observers = []}) ->
    ok;
broadcast_status_update(#state{observers = Pids, circuts = Circuts}) ->
    Data =
        lists:map(fun(#circut{status = Status, name = Name}) -> {Name, Status} end, Circuts),
    lists:foreach(fun(Pid) -> Pid ! {status_update, Data} end, Pids),
    ok.
