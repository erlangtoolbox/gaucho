%% Copyright
-module(test_web).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([f1/2, f2/1]).

-webmethod({"/f1/{email}/{login}", [get], "text/plain", auto, [
    {email, path},
    {login, path}
]}).
-spec(f1(string(), string()) -> error_m:monad(string())).
f1(Email, Login) -> {ok, {Email, Login}}.

-webmethod({"/f2/{email:\\d\\d}/1", [get], "text/plain", auto, [{email, path}]}).
-spec(f2(string()) -> error_m:monad(string())).
f2(Email) -> {ok, Email}.
