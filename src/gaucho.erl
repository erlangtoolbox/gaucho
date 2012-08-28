-module(gaucho).

-include("route.hrl").


-export([process/4, parse_transform/2]).

-spec parse_transform/2 :: (list(), list()) -> list().

parse_transform(Forms, Options ) ->
    gaucho_webmethod:parse_transform(Forms, Options).

-spec transform/2 :: (string()|list()|integer()|binary()|float(), atom()) -> any().
transform(Value, To) ->
    Function = list_to_atom(string:concat("to_", atom_to_list(To))),
    apply(xl_strng, Function, [Value]).

parse_body(_, _, <<>>, _) ->
    undefined;

parse_body(ContentType, ModuleName, Body, RecordName) ->
    Descr = ctparse:init(ContentType, ModuleName),
    ctparse:from(Descr, Body, RecordName).
    
%%apply(ParserName, from_json, [Body, RecordName]).





get_attributes(Req, PathVariables, Attributes) ->
        get_attributes(Req, PathVariables, [], Attributes).

get_attributes(_Req, PathVariables, _Acc, 
	       [{{Name, {body, {ContentType, ParserName}}}, Spec}| Attributes]) ->
    {ok, Body, Req} = cowboy_http_req:body(_Req),
    Json = case Spec of
	       {record, Type} -> 
		   {ok, Result} = parse_body(ContentType, ParserName, Body, Type),
		   Result;
	       {maybe, {record, Type}} ->
		   parse_body(ContentType, ParserName, Body, Type)
	   end,
    Acc = lists:append(_Acc, [{Name, Json}]),
    get_attributes(Req, PathVariables, Acc, Attributes);

get_attributes(_Req, PathVariables, _Acc, [{{Name, Spec}, AttributeType}| Attributes]) 
  when is_atom(AttributeType) ->
    %% io:format("AttributeType: ~p~n", [AttributeType]),
    {Value, Req} = 
	case Spec of
	    path ->
		{Name, _Value} = lists:keyfind(Name, 1, PathVariables),
		{_Value, _Req};
	    %% Acc = lists:append(_Acc, [{Name, Value}]),
	    %% get_attributes(Req, PathVariables, Acc, Attributes);		
	    'query' ->
		cowboy_http_req:qs_val(atom_to_binary(Name, utf8), _Req);

	    cookie ->
		cowboy_http_req:cookie(atom_to_binary(Name, utf8), _Req);

	    header ->
		cowboy_http_req:header(Name, _Req);
	    _ -> {{Name, undefined}, _Req}

	end,
    CValue = transform(Value, AttributeType),
    Acc = lists:append(_Acc, [{Name, CValue}]),
    get_attributes(Req, PathVariables, Acc, Attributes);

get_attributes(_, _, Acc, _) ->
    Acc;
get_attributes(_, _, Acc, []) ->
    Acc.




%find handler for rawpath
process([Route|Routes], _Req, State,  Module) ->

    {RawPath, _} = cowboy_http_req:raw_path(_Req),
    {Path, _} = cowboy_http_req:path(_Req),
    %%io:format("RawPath: ~p~n", [RawPath]),
    %%io:format("Path: ~p~n", [Path]),

    case re:run(RawPath, Route#route.path, [{capture, all, list}]) of
	nomatch ->
	    process(Routes, _Req, State,  Module);
	{match, _} ->

	    Method = case cowboy_http_req:method(_Req) of
			 {RawMethod, _} when is_binary(RawMethod) ->
			     binary_to_atom(RawMethod, utf8);
			 {RawMethod, _} when is_atom(RawMethod) ->
			     list_to_atom(string:to_lower(atom_to_list(RawMethod)))
		     end,
	    %%io:format("METHOD: ~p~n", [Method]),
	    case lists:member(Method, Route#route.accepted_methods) of
		true ->
		    %% io:format("Route: ~p~n", [Route]),
		    PathVariables = extract_path_variables(_Req, Route),
		    Variables = get_attributes(_Req, PathVariables, Route#route.attribute_specs),
		    Attributes = [Val||{_, Val} <- Variables],
		    %% io:format("Attributes: ~p~n", [Attributes]),
		    case apply(Module, Route#route.handler, Attributes) of
			{ok, Result} when is_tuple(Result) or is_list(Result)->
			    %% io:format("BODY: ~p~n", [Body]),
			    %% Response = apply(element(1, Result), to_json, [Result]),
			    Response = prepare_response(Result, Route),
			    {ok, Req} = cowboy_http_req:reply(200, [], Response, _Req),
			    {ok, Req, 200};
			{error, {Status, Message}} -> 
			    {ok, Req} = cowboy_http_req:reply(Status, [], Message, _Req),
			    {ok, Req, Status};
			ok -> 
			    {ok, _Req, 204};
			{error, UnexpectedError} ->
			    io:format("~p~n", [UnexpectedError]),
			    {ok, Req} = cowboy_http_req:reply(404, [], <<"Not found">>, _Req),
			    {ok, Req, 404};
			UnexpectedResult  ->
			    Info = io_lib:format("~p~n", [UnexpectedResult]),
			    {ok, Req} = cowboy_http_req:reply(500, [], list_to_binary(Info), _Req),
			    {ok, Req, 500}
		    end;
		false -> 
		    process(Routes, _Req, State, Module)
	    end
    end;

process([], _Req, State, _) ->
    {ok, Req} = cowboy_http_req:reply(404, [], <<"">>, _Req),
    {ok, Req, State}.

prepare_response(Result, #route{produces={ContentType, ParserModuleName}, output_spec={record, _}}) ->
    Descr = ctparse:init(ContentType, ParserModuleName),
    ctparse:to(Descr, Result);
prepare_response(Result, #route{produces={ContentType, ParserModuleName}, output_spec={list, {record, Type}}})
  when is_list(Result) ->
    Descr = ctparse:init(ContentType, ParserModuleName),	    
    ParsedResult = [ctparse:to(Descr, Elem) || Elem <- Result],

    
    ResultString = xl_string:join(ParsedResult, <<",">>),
    <<"[", ResultString/binary, "]">>;
prepare_response(Result, Route) ->
    erlang:error(gaucho_not_implemented, [Result, Route]).


extract_path_variables(Req, Route) ->
    case re:run(Route#route.raw_path, "{([^/:]*):?[^/]*}", [global, {capture, [1], list}]) of
	{match, VariableNames} ->
	    Keys = [list_to_atom(Name)||[Name] <- VariableNames],
	    {RawPath, _} = cowboy_http_req:raw_path(Req),
	    {match, [_|Values]} = re:run(RawPath, Route#route.path, [{capture, all, list}]),
	    lists:zip(Keys, Values);
	nomatch -> []
    end.



fill_path_variables(Variables, [PathVariable = {Key, _}| PathVariables]) ->
    fill_path_variables(lists:keyreplace(Key, 1, Variables, PathVariable), PathVariables);

fill_path_variables(Variables, []) ->
    Variables.


 %replace placeholders by its regexes
replace_ph(Subject, [Head| Replacements]) ->
    Search = lists:nth(1, Head),
    Replace = case Elem = lists:nth(3, Head) of
		  [] -> "([^/]+)"; 
		  _ -> Elem
	      end,
    replace_ph(str_replace(Subject, Search, Replace), Replacements);

replace_ph(Subject, []) ->
    Subject.



%replace Search with Replace in Subject
str_replace(Subject, Search, Replace) ->
    Lstr = string:len(Subject),
    Lsearch = string:len(Search),
    Pos = string:str(Subject, Search),
    if
	Pos =:= 0 -> Subject;
	true -> 
	    LeftPart = string:left(Subject, Pos-1),
	    RightPart = string:right(Subject, Lstr-Lsearch-Pos+1),
	    string:concat(string:concat(LeftPart, Replace), RightPart)
    end.

