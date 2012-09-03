-module(gaucho_test_converter).
-behaviour(gaucho_converter).

-include("test.hrl").
-export([from/3, to/3]).

from(Body, _ContentType, Spec) ->
    io:format("Spec: ~p~n", [Spec]),
    [Id, Name] = string:tokens(xl_string:to_string(Body),";"),
    {ok, #test{id=Id,name=Name}}.

to(_Result, _ContentType, OutputSpec) ->
    io:format("OutputSpec: ~p~n", [OutputSpec]),
    <<"Blablabla">>.
