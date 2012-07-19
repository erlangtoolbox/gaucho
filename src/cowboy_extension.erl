-module(cowboy_extension).
-include("route.hrl").


-export([process/4, prepare_route/1]).

get_attributes(Req, Attributes) ->
    get_attributes(Req, [], Attributes).

get_attributes(Req, Acc, [{Name, {body, json, ParserName}}|Attributes]) ->
    {ok, Body, Req1} = cowboy_http_req:body(Req),
    {ok, Json} = apply(ParserName, from_json, [Body,ParserName]),
    Acc1 = lists:append(Acc, [{Name, Json}]),
    get_attributes(Req1, Acc1, Attributes);

get_attributes(Req, Acc, [{Name, path}|Attributes]) ->
    Acc1 = lists:append(Acc, [{Name, undefined}]),
    get_attributes(Req, Acc1, Attributes);

get_attributes(Req, Acc, [{Name, 'query'}|Attributes]) ->
    {Value, Req1} = cowboy_http_req:qs_val(atom_to_binary(Name, utf8), Req),
    Acc1 = lists:append(Acc, [{Name, Value}]),
    get_attributes(Req1, Acc1, Attributes);

get_attributes(Req, Acc, [{Name, _}|Attributes]) ->
    Acc1 = lists:append(Acc, [{Name, nil}]),
    get_attributes(Req, Acc1, Attributes);

get_attributes(_, Acc, []) ->
    Acc.



%find handler for rawpath
process([Route|Routes], Req, State,  Module) ->

    {RawPath, _} = cowboy_http_req:raw_path(Req),

    case re:run(RawPath, Route#route.path, [{capture, all, list}]) of
	nomatch ->
	    process(Routes, Req, State,  Module);
	{match, _} ->
	    {Method, _} = cowboy_http_req:method(Req),
	    case lists:member(binary_to_atom(Method, utf8), Route#route.accepted_methods) of
		true ->
		    Variables = get_attributes(Req, Route#route.attribute_sources),
		    AllVariables  = 
			case lists:keymember(path, 2, Route#route.attribute_sources) of
			    true ->
				PathVariables = extract_path_variables(Req, Route),
				fill_path_variables(Variables, PathVariables);
			    false ->
				Variables
			end,
		    Attributes = [Val||{_, Val} <- AllVariables],
		    io:format("~p~n", [Attributes]),
		    Result = apply(Module, Route#route.handler, Attributes),
		    io:format("RESULT: ~p~n", [Result]),
		    %TODO: Specify Result format
		    %TODO: Implement Result parsing
		    {ok, Req, State};
		false -> 
		    process(Routes, Req, State, Module)
	    end
    end;

process([], Req, State, _) ->
    {ok, Req1} = cowboy_http_req:reply(404, [], <<"">>, Req),
    {ok, Req1, State}.



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
    case re:run(
	   RoutePattern, "{([^/:]*):?([^/]*)}", 
	   [global, {capture, all, list}]
	  ) of
	{match, Replacements} ->
	    string:concat(
	      string:concat("^",
			    replace_ph(RoutePattern, Replacements)), 
	      "$");
	nomatch -> RoutePattern
    end.

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

