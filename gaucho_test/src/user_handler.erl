-module(user_handler).


-export([create/1, retrieve/1, delete/1, search/3]).

-behaviour(cowboy_http_handler).

-compile({parse_transform, gaucho}).

-include("user.hrl").

%curl -X post -d "email;mname" http://localhost:8080/user
-webmethod({
    "/user",
    [post],
    {"text/plain", gaucho_test_converter},
    auto,
    [{user, {body, {"text/plain", gaucho_test_converter}}}]
}).
-spec create/1 :: (#user{}) -> error_m:monad(#user{}).
create(User) ->
    {ok, User}.

%curl http://localhost:8080/user/email
-webmethod({
    "/user/{email}",
    [get],
    {"text/plain", gaucho_test_converter},
    auto,
    [{email, path}]
}).
-spec retrieve/1 :: (string()) -> error_m:monad(#user{}).
retrieve(Email) ->
    {ok, #user{email=Email, name="Name"}}.

%curl -x delete http://localhost:8080/user/email
-webmethod({
    "/user/{email}",
    [delete],
    {"text/plain"},
    raw,
    [{email, path}]
}).
-spec delete/1 :: (binary()) -> error_m:monad(any()).
delete(Email) ->
    {ok, <<"User with email: '", Email/binary, "' deleted.">>}.


-webmethod({
    "/user/search/{email}",
    [get],
    {"text/plain"},
    auto,
    [{email, path}, {field, 'query'}, {value, 'query'}]
}).
-spec search/3 :: (binary(), binary(), binary()) -> error_m:monad(any()).
search(Email, Field, Value) ->
    {ok, <<Email/binary, ": ",Field/binary, " = ", Value/binary>>}.
