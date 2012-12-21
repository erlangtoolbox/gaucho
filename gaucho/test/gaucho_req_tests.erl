-module(gaucho_req_tests).
-author("Dmitry Kasimtsev <dmitry.kasimtsev@strikead.com>").


-include_lib("eunit/include/eunit.hrl").
-include_lib("xl_stdlib/include/xl_eunit.hrl").

qs_vals_lowercase_test()->
    ?assertEquals(
        [{id, <<"vAluE">>}, {name, <<"dMYtrY">>}],
        gaucho_req:qs_vals_lowercase([{<<"iD">>, <<"vAluE">>},{<<"naME">>, <<"dMYtrY">>}])
    ).
