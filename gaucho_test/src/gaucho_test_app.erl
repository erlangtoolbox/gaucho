-module(gaucho_test_app).


-compile({parse_transform, do}).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    do([error_m ||
        Dispatch <- application:get_env(gaucho_test, dispatch),
        Listeners <- application:get_env(gaucho_test, listeners),
        gaucho:start(Dispatch, Listeners),
        gaucho_test_sup:start_link()
    ]).

stop(_State) ->
    ok.
