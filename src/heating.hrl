-type day() :: erlcron:dow() | null.
-type circut_status() :: idle | running | blocked.
-type planned_run() ::
    {Day :: day(), StartTime :: calendar:time(), Duration :: calendar:time()}.
-type millis() :: integer().

-record(circut,
        {name :: atom(),
         break_duration :: calendar:time(),
         running_duration :: calendar:time(),
         stop_timer_ref = null :: timer:tref() | null,
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
         observers = [] :: [pid()],
         temp_read_interval :: calendar:time(),
         pomp_pin :: integer(),
         boiler_thermometer_id :: string(),
         boiler_min_temp :: float(),
         boiler_temp :: float() | null}).
