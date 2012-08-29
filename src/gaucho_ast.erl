-module(gaucho_ast).
-compile(export_all).


%ast helpers
-spec build/2 :: (tuple()|list()|string()|integer()|float(), integer()) -> any().
build(Term, Line) ->
    Str = lists:flatten(io_lib:format(
			   string:concat(string:copies("~n", Line-1), "~p."), [Term])),
    {ok, Ts, _} = erl_scan:string(Str),
    {ok, [Result]} = erl_parse:parse_exprs(Ts),
    Result.


-spec insert_before_first/3 :: (any(), any(), list()) -> list().
insert_before_first(BeforeElem, Elem, TupleList) ->
    {Head, Tail} = lists:splitwith(fun(E) ->
                case  E of
                    BeforeElem ->
                        false;
                    _ ->
                        true
                end
        end, TupleList),
    Head ++ [Elem] ++ Tail.
