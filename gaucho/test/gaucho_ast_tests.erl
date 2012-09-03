-module(gaucho_ast_tests).
-include_lib("eunit/include/eunit.hrl").

-record(user, {id, name, email}).

build_test()->
    ?assertEqual({atom, 10, user}, gaucho_ast:build(user, 10)),
    ?assertEqual({integer, 11, 11}, gaucho_ast:build(11, 11)),
    ?assertEqual({nil,12}, gaucho_ast:build([], 12)),
    ?assertEqual(
        {cons, 13, {integer, 13, 1}, 
            {cons, 13, {string, 13, "str"}, 
                {cons, 13, {atom, 13, atom},{nil, 13}}}}, 
        gaucho_ast:build([1 ,"str", atom], 13)).

