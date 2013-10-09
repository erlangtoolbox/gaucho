-module(swagger_converter).

-behaviour(gaucho_converter).

-export([from/3, to/3]).

to(Value, _ContentType, _Type) ->
    {ok, swagger:to_json(Value)}.

from(Value, _ContentType, _Type) ->
    {ok, swagger:from_json(Value)}.

