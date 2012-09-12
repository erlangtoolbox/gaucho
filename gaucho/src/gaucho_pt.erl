-module(gaucho_pt).

-export([parse_transform/2]).

-include("gaucho.hrl").
                

parse_transform(Forms, _Options) ->
    [ModuleName] = [element(4, Name) || Name <- Forms, element(3, Name) == module],
    {eof, Line} = lists:keyfind(eof, 1, Forms),
    Routes = extract_webmethods(Forms),
    RoutesAst= gaucho_ast:build(Routes, Line),
    Forms1 = lists:keydelete(eof, 1, Forms),

    HandleFunc = {function,Line,handle,2,
		  [{clause,Line,
		    [{var,Line,'Req'},{var,Line,'State'}],
		    [],
		    [{match,Line,
		      {var,Line,'Routes'}, RoutesAst},
		     {call,Line,
		      {remote,Line,{atom,Line,gaucho},{atom,Line,process}},
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

    FirstFun = {function, FLine, _Name, _Arity, _Clause} = lists:keyfind(function, 1, Forms2),
    Export = {attribute, FLine-1, export, [{init, 3}, {handle, 2}, {terminate, 2}]},
    FormsWithExport = xl_lists:insert_before(FirstFun, Export, Forms2),

    %% Handle = [Func || Func <- Forms2, element(1, Func) == function,element(3, Func) == handle],
    ResultForms = [Form|| Form <- FormsWithExport, element(3,Form) =/= webmethod],
    %io:format("~p~n", [ResultForms]),
    ResultForms.

    

 %replace placeholders by its regexes
replace_ph(Subject, [Head| Replacements]) ->
    Search = lists:nth(1, Head),
    Replace = case Elem = lists:nth(3, Head) of
		  [] -> "([^/]+)"; 
		  _ -> Elem
	      end,
    replace_ph(xl_string:replace(Subject, Search, Replace), Replacements);



%replace Search with Replace in Subject
replace_ph(Subject, []) ->
    Subject.

prepare_route(RoutePattern) ->
    Result = case re:run(
		    RoutePattern, "{([^/:]*):?([^/]*)}", 
		    [global, {capture, all, list}]
		   ) of
		 {match, Replacements} ->
		     replace_ph(RoutePattern, Replacements);

		 nomatch -> RoutePattern
	     end,
    string:concat(
      string:concat("^", Result),
      "$").

adopt_attribute_sources([{Name, Spec}|AttributeSources]) ->
    [#param{name=Name, from=Spec}|adopt_attribute_sources(AttributeSources)];
adopt_attribute_sources([{Name, Spec, Validators}|AttributeSources]) ->
    [#param{name=Name, from=Spec, validators=Validators}|adopt_attribute_sources(AttributeSources)];
adopt_attribute_sources([]) ->
    [].

extract_webmethods(Forms) ->
    extract_webmethods(looking_for_annotation, nil, [], Forms).

extract_webmethods(looking_for_handler, WebmethodOpts, Acc, 
		   [{attribute, _, spec, {{Name, _} , TypeSpecs}} |Forms]) ->
    Path = prepare_route(element(1, WebmethodOpts)),
    [{type, _, 'fun', [{type, _, product, Types}, Out]}] = TypeSpecs,
    AttributeTypes = gaucho_utils:get_attribute_types(Types),

    {_, HTTPMethods, Produces, OutFormat, AttributeSources} = WebmethodOpts,
    Route = #route{
      path=Path,
      handler=Name, 
      accepted_methods=HTTPMethods, 
      produces=Produces, 
      out_format=OutFormat, 
      attribute_specs=lists:zip(adopt_attribute_sources(AttributeSources), AttributeTypes),
      output_spec=gaucho_utils:get_output_type(Out),
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
