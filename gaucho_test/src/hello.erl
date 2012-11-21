-module(hello).
-compile({parse_transform, gaucho}).

-export([f1/0]).

-webmethod({"/user", [post], "text/plain", auto, []}).
-spec(f1() -> error_m:monad(string())).
f1() -> {ok, "hello"}.
