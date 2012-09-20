-module(user_handler).


%-export([create/1, retrieve/1, delete/1, search/4, email_validator/1]).
-compile(export_all).


-behaviour(cowboy_http_handler).

-compile({parse_transform, do}).
-compile({parse_transform, gaucho}).

-include("user.hrl").
-include_lib("gaucho/include/gaucho.hrl").

%curl -X post -d "email;mname" http://localhost:8080/user
-webmethod({
    "/user",
    [post],
    {"text/plain", gaucho_test_converter},
    auto, [{user, {body, {"text/plain", gaucho_test_converter}}}]
}).
-spec create/1 :: (#user{}) -> error_m:monad(#user{}).
create(User) ->
    {ok, User}.

-webmethod({
        "/user/test",
        [get],
        "text/plain",
        auto,
        [{id,'query'}]
    }).
-spec usr_tst/1 :: (option_m:monad(binary())) -> error_m:monad(any()).
usr_tst(Id) ->
    io:format("ID: ~p~n", [Id]),
    ok.
%curl http://localhost:8080/user/email
-webmethod({
    "/user/{email}",
    [get],
    {"text/plain", gaucho_test_converter},
    auto,
    [{email, path, [{?MODULE, email_validator}]}]
}).

-spec retrieve/1 :: (string()) -> error_m:monad(#user{}).
retrieve(Email) ->
    {ok, #user{email=Email, name="Name"}}.

%curl -x delete http://localhost:8080/user/email
-webmethod({
    "/user/{email}",
    [delete],
    "text/plain",
    raw,
    [{email, path}]
}).
-spec delete/1 :: (binary()) -> error_m:monad(any()).
delete(Email) ->
    {ok, <<"User with email: '", Email/binary, "' deleted.">>}.

%curl http://localhost:8080/user/search/something?field=name&value=Name
-webmethod({
    "/user/search/{email}",
    [get],
    "text/plain",
    auto,
    [{ip,ip}, {email, path}, {field, 'query'}, {value, 'query'}]
}).
-spec search/4 :: (binary(), binary(), binary(), binary()) -> error_m:monad(any()).
search(Ip, Email, Field, Value) ->
    {ok, <<"IP Address: '",Ip/binary,"' ",Email/binary, ": ",Field/binary, " = ", Value/binary>>}.


-spec email_validator/1 :: (string()) -> error_m:monad(any()).
email_validator(Value) ->
    case re:run(Value,"^[a-z0-9]*@[a-z0-9]*.[a-z0-9]{2,3}$", [global,{capture, all, list}]) of
            {match, _} ->
                ok;
            nomatch ->
                {error, invalid_email}
    end.
        
