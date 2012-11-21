%% Copyright
-module(gaucho_webmethod_tests).
-author("volodymyr.kyrychenko@strikead.com").

-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_stdlib/include/xl_eunit.hrl").
-include("gaucho_webmethod.hrl").

mapping_test() ->
    T = xl_eunit:resource(?MODULE, "test_web.erl.t"),
    {ok, Forms} = epp:parse_file(T, "./", []),
    ?assertEquals([
        #webmethod{
            path = <<"/f1/([^/]+)/([^/]+)$">>,
            http_methods = [get],
            produces = "text/plain",
            param_spec = [
                #webmethod_param{name = email, from = path, type = string},
                #webmethod_param{name = login, from = path, type = string}
            ],
            result_type = string,
            function = f1,
            raw_path = <<"/f1/{email}/{login}">>
        },
        #webmethod{
            path = <<"/f2/\\d\\d/1$">>,
            http_methods = [get],
            produces = "text/plain",
            param_spec = [#webmethod_param{name = email, from = path, type = string}],
            result_type = string,
            function = f2,
            raw_path = <<"/f2/{email:\\d\\d}/1">>
        }
    ], gaucho_webmethod:mapping(Forms)).

compile_path_test() ->
    ?assertEqual(<<"/f2/\\d\\d/1$">>, gaucho_webmethod:compile_path("/f2/{email:\\d\\d}/1")),
    ?assertEqual(<<"/f1/([^/]+)/([^/]+)$">>, gaucho_webmethod:compile_path("/f1/{email}/{login}")).

typeof_test() ->
    ?assertEqual(integer, gaucho_webmethod:typeof({type, 10, integer, []})),
    ?assertEqual({record, record_name}, gaucho_webmethod:typeof({type, 10, record, [{atom, 10, record_name}]})),
    ?assertEqual({option, {record, record_name}}, gaucho_webmethod:typeof({remote_type, 10, [
        {atom, 10, option_m},
        {atom, 10, monad},
        [{type, 10, record, [{atom, 10, record_name}]}]
    ]})).

result_type_test() ->
    ?assertEqual({list, {record, record_name}}, gaucho_webmethod:result_type(
        {type, 10, list, [{type, 10, record, [{atom, 10, record_name}]}]}
    )),
    ?assertEqual({list, string}, gaucho_webmethod:result_type({type, 10, list, [{type, 10, string, []}]})),
    ?assertEqual({record, record_name}, gaucho_webmethod:result_type({remote_type, 10, [
        {atom, 10, error_m},
        {atom, 10, monad},
        [{type, 10, record, [{atom, 10, record_name}]}]
    ]})).
