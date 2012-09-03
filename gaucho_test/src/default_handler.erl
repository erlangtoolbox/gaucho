-module(default_handler).


-export([get_number/1, post_test/1, testo_numero_uno/1]).

-behaviour(cowboy_http_handler).

-compile({parse_transform, gaucho}).

-include("test.hrl").

-webmethod({
    "/test/{number}",
    [get],
    {"text/plain"},
    auto,
    [{number, path}]
}).
-spec get_number/1 :: (integer()) -> error_m:monad(integer()).
get_number(Number) ->
    {ok, Number}.

-webmethod({
        "/test",
        [post],
        {"text/plain"},
        auto,
        [{req, {body, "text/plain"}}]
    }).
-spec   post_test/1 :: (string()) -> error_m:monad(string()).
post_test(Req) when is_list(Req)->
    {ok, io_lib:format("This is a string '~s'~n", [Req])}.

-webmethod({
        "/test/test",
        [post],
        {"text/plain", gaucho_test_converter},
        auto,
        [{req, {body, {"text/plain", gaucho_test_converter}}}]
    }).
-spec testo_numero_uno/1 :: (#test{}) -> error_m:monad(list(string())).
testo_numero_uno(Req) ->
    io:format("Req in testo_numero_uno: '~p'~n", [Req]),
    {ok, Req}.
