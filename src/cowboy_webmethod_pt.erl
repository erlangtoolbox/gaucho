-module(cowboy_webmethod_pt).
-export([parse_transform/2]).

-include("route.hrl").
build_any_ast(Term, Line) ->
    Str = lists:flatten(io_lib:format(
			   string:concat(string:copies("~n", Line-1), "~p."), [Term])),
    {ok, Ts, _} = erl_scan:string(Str),
    erl_parse:parse_exprs(Ts).

build_routes_ast(Routes, Line) ->
    {ok, [RoutesAst]} = build_any_ast(Routes, Line),
    RoutesAst.

parse_transform(Forms, _Options) ->
    [ModuleName] = [element(4, Name) || Name <- Forms, element(3, Name) == module],
    {eof, Line} = lists:keyfind(eof, 1, Forms),
    Routes = extract_webmethods(Forms),
						%    io:format("Routes: ~p~n", [Routes]),
    RoutesAst = build_routes_ast(Routes, Line),
    Forms1 = lists:keydelete(eof, 1, Forms),

    HandleFunc = {function,Line,handle,2,
		  [{clause,Line,
		    [{var,Line,'Req'},{var,Line,'State'}],
		    [],
		    [{match,Line,
		      {var,Line,'Routes'}, RoutesAst},
		     {call,Line,
		      {remote,Line,{atom,Line,cowboy_extension},{atom,Line,process}},
		      [{var,Line,'Routes'},
		       {var,Line,'Req'},
		       {var,Line,'State'},
		       {atom,Line, ModuleName}]}]}]},
    InitFunc = {function,Line+1,init,3,
		[{clause,Line+1,
		  [{tuple,Line+1,[{var,Line+1,'_Any'},{atom,Line+1,http}]},{var,Line+1,'Req'},{nil,Line+1}],
		  [],
		  [{tuple,Line+1,[{atom,Line+1,ok},{var,Line+1,'Req'},{atom,Line+1,undefined}]}]}]},
    TerminateFunc = {function,Line+2,terminate,2,
		     [{clause,Line+2,[{var,Line+2,'_Req'},{var,Line+2,'_State'}],[],[{atom,Line+2,ok}]}]},
    Forms2 = lists:append(Forms1, [HandleFunc, InitFunc, TerminateFunc, {eof, Line+3}]),
						%Handle = [Func || Func <- Forms2, element(1, Func) == function,element(3, Func) == handle],
						%io:format("~p~n", [Handle]),
    [Form|| Form <- Forms2, element(3,Form) =/= webmethod].


extract_webmethods(Forms) ->
    extract_webmethods(looking_for_annotation, nil, [], Forms).


extract_webmethods(looking_for_handler, WebmethodOpts, Acc, [Form|Forms]) ->
    case Form of
	{function, _, Name, _, _} -> 
	    Path = cowboy_extension:prepare_route(element(1, WebmethodOpts)),
	    
	    {_, HTTPMethods, Produces, Output, AttributeSources} = WebmethodOpts,
	    Route = #route{
	      path=Path,
	      handler=Name, 
	      accepted_methods=HTTPMethods, 
	      produces=Produces, 
	      output=Output, 
	      attribute_sources=AttributeSources,
	      raw_path=element(1,WebmethodOpts)
	     },
	    Acc1 = lists:append(Acc, [Route]),
	    extract_webmethods(looking_for_annotation, nil, Acc1, Forms);
	_ ->
	    extract_webmethods(looking_for_handler, WebmethodOpts, Acc, Forms)
    end;
extract_webmethods(looking_for_annotation, _, Acc, [Form|Forms]) ->
    case Form of
	{attribute, _, webmethod, Opts} ->
	    %io:format("Opts: ~p~n", [ Opts ]),
	    extract_webmethods(looking_for_handler, Opts, Acc, Forms);
	_ -> extract_webmethods(looking_for_annotation, nil, Acc, Forms)
    end;
%%TODO: add type conversion
extract_webmethods(_, _, Acc, []) ->
    Acc.


