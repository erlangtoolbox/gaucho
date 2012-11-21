-module(gaucho_converter).

-callback(to(term(), string(), term()) -> error_m:monad(term())).
-callback(from(term(), string(), term()) -> error_m:monad(term())).
