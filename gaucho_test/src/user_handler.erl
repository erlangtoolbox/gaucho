-module(user_handler).


%-export([create/1, retrieve/1, delete/1, search/4, email_validator/1]).
-compile(export_all).


-behaviour(cowboy_http_handler).

-compile({parse_transform, do}).
-compile({parse_transform, gaucho}).

-include("user.hrl").


%curl localhost:8080/user/app_handler_opts
-webmethod({
    "/user/app_handler_opts",
    [get],
    "text/plain",
    raw,
    [{field1, state}]
}).
-spec(app_handler_opts(binary()) -> error_m:monad(any())).
app_handler_opts(Opt) ->
    {ok, Opt}.
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

-webmethod({"/user/header", [get], "text/plain", auto, [
    {'user-agent', header},
    {'some-header', header}
]}).
-spec usr_header/2 :: (binary(), option_m:monad(binary())) -> error_m:monad(string()).
usr_header(UA, SH) ->
    {ok, xl_string:format("UA: ~p, Other: ~p~n", [UA, SH])}.

-webmethod({ "/user/uri",
    [get],
    "text/plain",
    auto,
    [{uri, request_uri}]
}).
-spec usr_uri/1 :: (option_m:monad(binary())) -> error_m:monad(any()).
usr_uri(Uri) ->
    io:format("Request URI: ~p~n", [Uri]),
    ok.

-webmethod({
    "/user/qry",
    [get],
    "text/plain",
    auto,
    [{id, 'query'}, {name, 'query'}]
}).
-spec usr_qry/2 :: (binary(), binary()) -> error_m:monad(any()).
usr_qry(Id, Name) ->
    io:format("Query string: {id: ~p, name: ~p}~n", [Id, Name]),
    ok.


-webmethod({"/user/redirect", [get], "text/plain", auto, []}).
-spec(redirect() -> error_m:monad(binary())).
redirect() -> {ok, {302, <<"http://google.com">>}}.

-webmethod({
    "/user/cookie",
    [get],
    "text/plain",
    auto,
    [{id, cookie}, {name, cookie}]
}).
-spec usr_cookie/2 :: (binary(), binary()) -> error_m:monad(any()).
usr_cookie(Id, Name) ->
    io:format("Cookie string: {id: ~p, name: ~p}~n", [Id, Name]),
    ok.

%curl http://localhost:8080/user/ip
-webmethod({
    "/user/ip",
    [get],
    "text/plain",
    auto,
    [{ip, ip}]
}).
-spec usr_ip/1 :: (binary()) -> error_m:monad(any()).
usr_ip(Ip) ->
    io:format("Ip:~p~n", [Ip]),
    {ok, Ip}.
-webmethod({
    "/user/host",
    [get],
    "text/plain",
    auto,
    [{host, host}]
}).
-spec usr_host/1 :: ({}) -> error_m:monad(any()).
usr_host(Req) ->
    io:format("host:~p~n", [Req]),
    {ok, Req}.
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
    {ok, #user{email = Email, name = "Name"}}.

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
    {ok, <<"User with email: '", Email / binary, "' deleted.">>}.


-spec email_validator/1 :: (string()) -> error_m:monad(any()).
email_validator(Value) ->
    case re:run(Value, "^[a-z0-9]*@[a-z0-9]*.[a-z0-9]{2,3}$", [global, {capture, all, list}]) of
        {match, _} ->
            ok;
        nomatch ->
            {error, invalid_email}
    end.
        

