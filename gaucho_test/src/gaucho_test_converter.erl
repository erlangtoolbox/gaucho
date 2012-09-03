-module(gaucho_test_converter).
-behaviour(gaucho_converter).

-export([from/3, to/3]).

from(Body, _ContentType, _OutputSpec) ->
    string:tokens(xl_string:to_string(Body),";").

to(_Result, _ContentType, OutputSpec) ->
    io:format("OutputSpec: ~p~n", [OutputSpec]),
    <<"Blablabla">>.
