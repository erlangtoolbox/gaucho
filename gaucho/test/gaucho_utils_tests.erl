-module(gaucho_utils_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

get_attribute_type_test() ->
    % integer()
    ?assertEqual(integer, gaucho_utils:get_attribute_type({type, 10, integer, []})),
    % #record_name{}
    ?assertEqual({record, record_name}, 
        gaucho_utils:get_attribute_type({type, 10, record, [{atom, 10, record_name}]})),
    % option_m:monad(#record_name{})
    ?assertEqual({maybe, {record, record_name}},
        gaucho_utils:get_attribute_type(
            {remote_type, 10, 
                [{atom, 10, option_m},
                    {atom, 10, monad},
                    [{type, 10, record, [{atom, 10, record_name}]}]]})),
    try
        % list() it's denied as an input format
        gaucho_utils:get_attribute_type({type, 10, list, []})
    catch
        _:unsupported_type -> ok
    end,
    try
        % list(#record_name{}) it's denied as an input format
        gaucho_utils:get_attribute_type({type, 10, list, [{type, 10, record, [{atom, 10, record_name}]}]})
    catch
        _:unsupported_type -> ok
    end.

get_output_type_test() ->
    % list(#record_name{}) 
    ?assertEqual( {list, {record, record_name}},
        gaucho_utils:get_output_type({type, 10, list, [{type, 10, record, [{atom, 10, record_name}]}]})),
    % list(string())
    ?assertEqual( {list, string},
        gaucho_utils:get_output_type({type, 10, list, [{type, 10, string, []}]})),
    % error_m:monad(#record_name{})
    ?assertEqual({record, record_name},
        gaucho_utils:get_output_type(
            {remote_type, 10, 
                [{atom, 10, error_m},
                    {atom, 10, monad},
                    [{type, 10, record, [{atom, 10, record_name}]}]]})).
