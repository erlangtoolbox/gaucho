-module(swagger_converter).

-behaviour(gaucho_converter).

-export([from/3, to/3]).

to(Value, _ContentType, _Type) ->
    swagger:to_json(Value).

from(Value, _ContentType, _Type) ->
    swagger:from_json(Value).

