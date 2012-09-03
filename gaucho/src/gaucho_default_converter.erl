-module(gaucho_default_converter).
-behaviour(gaucho_converter).

-export([from/3, to/3]).

to(Result, _ContentType, _OutSpec)->
    {ok, xl_string:to_binary(Result)}.

from(Body, _ContentType, OutputSpec) ->
    {ok, apply(xl_string, xl_string:mk_atom(["to_", OutputSpec]), [Body])}.
