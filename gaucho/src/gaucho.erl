-module(gaucho).

-include("route.hrl").


-export([process/4, parse_transform/2, start/2]).

-spec parse_transform/2 :: (list(), list()) -> list().

parse_transform(Forms, Options ) ->
    gaucho_pt:parse_transform(Forms, Options).

-spec transform/2 :: (string()|list()|integer()|binary()|float(), atom()) -> any().
transform(Value, To) ->
    Function = list_to_atom(string:concat("to_", atom_to_list(To))),
    apply(xl_string, Function, [Value]).






get_attributes(Req, PathVariables, Attributes) ->
        get_attributes(Req, PathVariables, [], Attributes).

get_attributes(_Req, PathVariables, Acc, 
        [{{Name, {body, {ContentType, Converter}}}, Spec}| Attributes]) ->

        {ok, Body, Req} = cowboy_http_req:body(_Req),
        Content = Converter:from(Body, ContentType, Spec),
        Acc1 = lists:append(Acc, [{Name, Content}]),
        get_attributes(Req, PathVariables, Acc1, Attributes);

get_attributes(_Req, PathVariables, Acc, 
        [{{Name, {body, ContentType}}, Spec}| Attributes]) ->

        {ok, Body, Req} = cowboy_http_req:body(_Req),
        Content = gaucho_default_converter:from(Body, ContentType, Spec),
        Acc1 = lists:append(Acc, [{Name, Content}]),
        get_attributes(Req, PathVariables, Acc1, Attributes);

get_attributes(_Req, PathVariables, _Acc, [{{Name, Spec}, AttributeType}| Attributes]) 
  when is_atom(AttributeType) ->
    {Value, Req} = 
	case Spec of
        body ->
            {ok, Body, Req1} = cowboy_http_req:body(_Req),
            {Body, Req1};
        path ->
            {Name, _Value} = lists:keyfind(Name, 1, PathVariables),
            {_Value, _Req};
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
    Acc.




%find handler for rawpath
process([Route|Routes], _Req, State,  Module) ->

    {RawPath, _} = cowboy_http_req:raw_path(_Req),
    {Path, _} = cowboy_http_req:path(_Req),

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
	    case lists:member(Method, Route#route.accepted_methods) of
		true ->
		    PathVariables = extract_path_variables(_Req, Route),
		    Variables = get_attributes(_Req, PathVariables, Route#route.attribute_specs),
		    Attributes = [Val||{_, Val} <- Variables],
		    case apply(Module, Route#route.handler, Attributes) of
			{ok, Result} ->
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

prepare_response(Result, #route{out_format=raw}) ->
    Result;
prepare_response(Result, #route{output_spec=OutputSpec,produces={ContentType, Converter},out_format=auto})->
    Converter:to(Result, ContentType, OutputSpec);
prepare_response(Result, R = #route{produces=ContentType, out_format=auto})->
    prepare_response(Result, R#route{produces={ContentType, gaucho_default_converter}}).

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


-spec start/2 :: (term(), [{atom(), pos_integer(), atom(), [term()], atom(), [term()]}]) -> error_m:monad(ok).
start(Dispatch, Listeners) ->
    xl_lists:eforeach(fun({Name, Acceptors, Transport, TransportOpts, Protocol, ProtocolOpts}) ->
        cowboy:start_listener(Name, Acceptors, Transport, TransportOpts, Protocol, [{dispatch, Dispatch} | ProtocolOpts])
    end, Listeners).




