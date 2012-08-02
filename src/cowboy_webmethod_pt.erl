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
    %% Handle = [Func || Func <- Forms2, element(1, Func) == function,element(3, Func) == handle],
    %% io:format("~p~n", [Handle]),
    [Form|| Form <- Forms2, element(3,Form) =/= webmethod].

get_attribute_types([Spec | Specs]) ->
    [get_attribute_type(Spec)| get_attribute_types(Specs)];

get_attribute_types([]) -> [].



get_attribute_type({remote_type,_, [{atom,_, maybe_m},{atom,_,monad},[Type]]}) ->
    {maybe, get_attribute_type(Type)};

get_attribute_type({type, _, SimpleType, []}) ->
    SimpleType;

get_attribute_type({type, _, record, [{atom, _, RecordName}]}) ->
    {record, RecordName};

get_attribute_type(UnsupportedType) ->
    erlang:error(cowboy_ext_unsupported_type, [UnsupportedType]).



extract_webmethods(Forms) ->
    extract_webmethods(looking_for_annotation, nil, [], Forms).

extract_webmethods(looking_for_handler, WebmethodOpts, Acc, 
		   [{attribute, _, spec, {{Name, _} , TypeSpecs}} |Forms]) ->
    Path = cowboy_extension:prepare_route(element(1, WebmethodOpts)),
    [{type, _, 'fun', [{type, _, product, Types}, Out]}] = TypeSpecs,
    io:format("Types: ~p~n", [Types]),
    AttributeTypes = get_attribute_types(Types),

    {_, HTTPMethods, Produces, OutFormat, AttributeSources} = WebmethodOpts,
    io:format("Name: ~p~n", [Name]),
    io:format("AttributeTypes: ~p~n", [AttributeTypes]),
    io:format("Out: ~p~n", [Out]),

    io:format("Alltogether: ~p~n~n", [lists:zip(AttributeSources, AttributeTypes)]),
    Route = #route{
      path=Path,
      handler=Name, 
      accepted_methods=HTTPMethods, 
      produces=Produces, 
      out_format=OutFormat, 
      attribute_specs=lists:zip(AttributeSources, AttributeTypes),
      raw_path=element(1,WebmethodOpts)
     },
    Acc1 = lists:append(Acc, [Route]),
    extract_webmethods(looking_for_annotation, nil, Acc1, Forms);

extract_webmethods(looking_for_handler, WebmethodOpts, Acc, [_| Forms]) ->
    extract_webmethods(looking_for_handler, WebmethodOpts, Acc, Forms);

extract_webmethods(looking_for_annotation, _, Acc, [{attribute, _, webmethod, Opts}|Forms]) ->
    %% io:format("Opts: ~p~n", [ Opts ]),
    extract_webmethods(looking_for_handler, Opts, Acc, Forms);

extract_webmethods(looking_for_annotation, _, Acc, [_| Forms]) ->
    extract_webmethods(looking_for_annotation, nil, Acc, Forms);

extract_webmethods(_, _, Acc, []) ->
    Acc.

%% functionblabla() ->
%%     {attribute,78,spec,
%%      {{get_field,3},
%%       [{type,78,'fun',
%% 	[{type,78,product,
%% 	  [{type,78,integer,[]},{type,78,atom,[]},{type,78,string,[]}]},
%% 	 {remote_type,78,
%% 	  [{atom,78,error_m},{atom,78,monad},[{type,78,any,[]}]]}]}]}}.
