-module(cowboy_extension).
-include("route.hrl").


-export([process/4, prepare_route/1]).

get_attributes(Req, Attributes) ->
    get_attributes(Req, [], Attributes).

get_attributes(_Req, _Acc, [{Name, Spec}| Attributes]) ->
    {Value, Req} = 
	case Spec of
	    {body, json, ParserName} ->
		{ok, Body, __Req} = cowboy_http_req:body(_Req),
		{ok, Json} = apply(ParserName, from_json, [Body,ParserName]),
		{Json, __Req};

	    path -> 
		{undefined, _Req};

	    'query' ->
		cowboy_http_req:qs_val(atom_to_binary(Name, utf8), _Req);

	    cookie ->
		cowboy_http_req:cookie(atom_to_binary(Name, utf8), _Req);

	    header ->
		cowboy_http_req:header(Name, _Req);
	    _ -> {{Name, undefined}, _Req}

	end,

    Acc = lists:append(_Acc, [{Name, Value}]),
    get_attributes(Req, Acc, Attributes);

get_attributes(_, Acc, []) ->
    Acc.



%find handler for rawpath
process([Route|Routes], _Req, State,  Module) ->
    %io:format("Route: ~p~n", [Route]),
    {RawPath, _} = cowboy_http_req:raw_path(_Req),

    case re:run(RawPath, Route#route.path, [{capture, all, list}]) of
	nomatch ->
	    process(Routes, _Req, State,  Module);
	{match, _} ->
	    %io:format("RawPath: ~p~n", [RawPath]),
	    {Method, _} = cowboy_http_req:method(_Req),
	    case lists:member(binary_to_atom(Method, utf8), Route#route.accepted_methods) of
		true ->
		    Variables = get_attributes(_Req, Route#route.attribute_sources),
		    AllVariables  = 
			case lists:keymember(path, 2, Route#route.attribute_sources) of
			    true ->
				PathVariables = extract_path_variables(_Req, Route),
				fill_path_variables(Variables, PathVariables);
			    false ->
				Variables
			end,
		    Attributes = [Val||{_, Val} <- AllVariables],
		    io:format("~p~n", [Attributes]),
		    case apply(Module, Route#route.handler, Attributes) of
			{ok, Body} when is_tuple(Body) ->
			    io:format("BODY: ~p~n", [Body]),
			    Json = apply(element(1, Body), to_json, [Body]),
			    {ok, Req} = cowboy_http_req:reply(200, [], Json, _Req),
			    {ok, Req, 200};
			{error, {Status, Message}} -> 
			    {ok, Req} = cowboy_http_req:reply(Status, [], Message, _Req),
			    {ok, Req, Status}
		    end;
		false -> 
		    process(Routes, _Req, State, Module)
	    end
    end;

process([], _Req, State, _) ->
    {ok, Req} = cowboy_http_req:reply(404, [], <<"">>, _Req),
    {ok, Req, State}.




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
      string:concat("^",
		    Result),
      "$").



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

