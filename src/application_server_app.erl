%%%-------------------------------------------------------------------
%% @doc application_server public API
%% @end
%%%-------------------------------------------------------------------

-module(application_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
