-module(gaucho_ast).
-compile(export_all).


%transform any term except functions to abstract syntax tree
-spec build/2 :: (tuple()|list()|string()|integer()|float(), integer()) -> any().
build(Term, Line) ->
    Str = lists:flatten(io_lib:format(
			   string:concat(string:copies("~n", Line-1), "~p."), [Term])),
    {ok, Ts, _} = erl_scan:string(Str),
    {ok, [Result]} = erl_parse:parse_exprs(Ts),
    Result.
