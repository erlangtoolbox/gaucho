-module(gaucho_validator_utils).
-author("Dmitry Kasimtsev <dmitry.kasimtsev@strikead.com>").

-export([errors_filter/1, run_validators/2, run_validators/3, errors_join/1]).

%[ok, {error, E1}, ok,..., {error, En}] => [{error, E1},...,{error, En}]
%[ok, ok, ok, ok] => ok

-spec errors_filter/1 :: (list(error_m:monad(ok))) -> list({error, string()}).
errors_filter([]) -> ok;
errors_filter(ResultsList) -> 
    case lists:filter(fun(Elem) -> case Elem of ok -> false; _Error -> true end end, ResultsList) of
            [] -> ok;
            Errors -> Errors 
    end.


-spec errors_join/1 :: (list()) -> error_m:monad(ok|string()).
errors_join([]) -> ok;
errors_join(ResultsList) -> 
    % think about remove ok clause from case
    case lists:filter(fun(Elem) -> case Elem of ok -> false; _Error -> true end end, ResultsList) of
            [] -> ok;
            Errors -> {error, [Message ||{error, Message} <- Errors]}
    end.


run_validators(Subject, ValidatorsList) ->
    run_validators(Subject, ValidatorsList, fun errors_join/1).



run_validators(Subject, ValidatorsList, CompactMethod) ->
    CompactMethod(lists:foldr(fun(Validator, AccIn) ->
                case Validator(Subject) of 
                    Err = {error, _Message} -> [Err|AccIn];
                    Errors when is_list(Errors) -> lists:append([AccIn, Errors]);
                    _ -> AccIn
                end
        end,
        [], ValidatorsList)).



