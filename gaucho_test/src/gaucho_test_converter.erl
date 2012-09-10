-module(gaucho_test_converter).
-behaviour(gaucho_converter).

-include("user.hrl").
-export([from/3, to/3]).

from(Body, _ContentType, {record, user}) ->
    [Email, Name] = string:tokens(xl_convert:to_string(Body),";"),
    {ok, #user{email=Email,name=Name}}.

to(Result, _ContentType, {record, user}) ->
    {ok, xl_convert:to_binary(xl_string:join([Result#user.email, Result#user.name], ";"))}.
