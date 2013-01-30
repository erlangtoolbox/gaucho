-module(gaucho_default_converter).

-behaviour(gaucho_converter).

-export([from/3, to/3]).

to(Value, _ContentType, _Type) ->
    try
        {ok, xl_convert:to(binary, Value)}
    catch
        _:_ -> {error, xl_string:format("cannot cast ~p to ~p", [Value, binary])}
    end.

from(undefined, _ContentType, Type) when is_atom(Type)->
    {error, xl_string:format("Mandatory parameter can\'t be undefined. Type ~p~n", [Type])};
from(Value, _ContentType, Type) when is_atom(Type) ->
    try
        {ok, xl_convert:to(Type, Value)}
    catch
        _:_ -> {error, xl_string:format("cannot cast ~p to ~p", [Value, Type])}
    end;
from(undefined, _ContentType, {option, _Type}) -> {ok, undefined};
from(Value, ContentType, {option, Type}) ->
    case from(Value, ContentType, Type) of
        {ok, X} -> {ok, {ok, X}};
        E -> E
    end.

