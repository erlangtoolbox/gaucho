-module(gaucho_validator_utils_tests).

-author("Dmitry Kasimtsev <dmitry.kasimtsev@strikead.com>").

-include_lib("eunit/include/eunit.hrl").
%-include_lib("xl_eunit/include/xl_eunit.hrl").

errors_filter_test() ->
    ?assertEqual(ok, gaucho_validator_utils:errors_filter([ok, ok, ok])),
    ?assertEqual([{error, e1}, {error, e2}, {error, e3}], gaucho_validator_utils:errors_filter([{error, e1}, ok, {error, e2}, ok, ok, {error, e3}, ok, ok, ok])).


errors_join_test() ->
    ?assertEqual(ok, gaucho_validator_utils:errors_join([ok, ok, ok])),
    ?assertEqual({error, [e1, e2, e3]}, gaucho_validator_utils:errors_join([{error, e1}, ok, {error, e2}, ok, ok, {error, e3}, ok, ok, ok])).


run_validators_simple_test() ->
    ?assertEqual(ok, gaucho_validator_utils:run_validators({}, [fun simple_validator_ok/1])),
    ?assertEqual({error, ["Error reason 1"]}, 
        gaucho_validator_utils:run_validators({}, [fun simple_validator_err/1])).


run_validators_complex_test() ->
    ?assertEqual(ok, gaucho_validator_utils:run_validators({}, [
                fun simple_validator_ok/1,
                fun complex_validator_ok/1])),
    ?assertEqual({error, ["Error reason 1", "Error reason 2", "Error reason 3"]}, 
            gaucho_validator_utils:run_validators({}, [
                    fun simple_validator_err/1,
                    fun complex_validator_err/1])),
    ?assertEqual({error, ["Error reason 1", "Error reason 2", "Error reason 3"]}, 
            gaucho_validator_utils:run_validators({}, [
                    fun simple_validator_ok/1, 
                    fun simple_validator_err/1, 
                    fun complex_validator_ok/1, 
                    fun complex_validator_err/1])).


run_validators_complex_filter_test() ->
    ?assertEqual([{error, "Error reason 1"}, {error, "Error reason 2"}, {error, "Error reason 3"}], 
            gaucho_validator_utils:run_validators({}, [
                    fun simple_validator_ok/1, 
                    fun simple_validator_err/1, 
                    fun complex_validator_ok/1, 
                    fun complex_validator_err/1], fun gaucho_validator_utils:errors_filter/1)).


simple_validator_ok(_) -> ok.


simple_validator_err(_) -> {error, "Error reason 1"}.


complex_validator_ok(_) -> [ok, ok, ok].


complex_validator_err(_) -> [ok, {error, "Error reason 2"}, ok, ok, {error, "Error reason 3"}, ok, ok, ok].
