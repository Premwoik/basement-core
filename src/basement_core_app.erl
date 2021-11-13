%%%-------------------------------------------------------------------
%% @doc basement_core public API
%% @end
%%%-------------------------------------------------------------------

-module(basement_core_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    basement_core_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
