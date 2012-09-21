-module(gaucho).

-include("gaucho.hrl").
-compile({parse_transform, do}).

-export([process/4, parse_transform/2, start/2]).

-spec parse_transform/2 :: (list(), list()) -> list().

parse_transform(Forms, Options ) ->
    gaucho_pt:parse_transform(Forms, Options).







get_content_type({ContentType, _Converter}) ->
    ContentType;
get_content_type(ContentType) ->
    ContentType.

%find handler for rawpath
process(AllRoutes = [Route|Routes], Req, State,  Module) ->
    {RawPath, _} = cowboy_req:path(Req),
    RRawPath = xl_convert:to_string(RawPath),
    {Path, _} = cowboy_req:path_info(Req),
    case lists:last(Path) of
        <<"_api">> ->
            {ok, Req1} = cowboy_req:reply(200, [], gaucho_utils:get_api(AllRoutes), Req),
            {ok, Req1, 200};
        _ ->
            Match = re:run(RRawPath, Route#route.path, [{capture, all, list}]),
            case Match of
                nomatch ->
                    process(Routes, Req, State,  Module);
                {match, _} ->
                    {RawMethod, _} = cowboy_req:method(Req),
                    Method = xl_convert:to_atom(xl_string:to_lower(RawMethod)),
                    case lists:member(Method, Route#route.accepted_methods) of
                        true ->
                            PathVariables = extract_path_variables(Req, Route),
                            
                            case gaucho_utils:get_attributes(Req, PathVariables, Route#route.attribute_specs) of
                                {ok, Variables} ->
                                    Attributes = [Val||{_, Val} <- Variables],
                                    %io:format("Produces: ~p~n", [Route#route.produces] ),
                                    case apply(Module, Route#route.handler, Attributes) of
                                        {ok, Result} ->
                                            {ok, Response} = prepare_response(Result, Route),
                                            ContentType = get_content_type(Route#route.produces),
                                            {ok, Req1} = cowboy_req:reply(200, [{<<"Content-Type">>, ContentType}], Response, Req),
                                            {ok, Req1, 200};
                                        ok -> 
                                            {ok, Req, 204};
                                        {error, Status} when is_integer(Status) ->
                                            {ok, Req1} = cowboy_req:reply(Status, Req),
                                            {ok, Req1, Status};
                                        {error, {Status, Message}} -> 
                                            {ok, Req1} = cowboy_req:reply(Status, [], Message, Req),
                                            {ok, Req1, Status};

                                        {error, UnexpectedError} ->
                                            io:format("~p~n", [UnexpectedError]),
                                            {ok, Req1} = cowboy_req:reply(404, Req),
                                            {ok, Req1, 404};
                                        UnexpectedResult  ->
                                            Info = io_lib:format("~p~n", [UnexpectedResult]),
                                            {ok, Resp} = cowboy_req:reply(500, [], xl_convert:to_binary(Info), Req),
                                            {ok, Resp, 500}
                                    end;
                                {error, Reason} when is_list(Reason) ->
                                    {ok, Resp} = cowboy_req:reply(400, [], xl_convert:to_binary(Reason), Req),
                                    {ok, Resp, 400};
                                {error, Reason} ->
                                    {ok, Resp} = cowboy_req:reply(400, [], xl_convert:to_binary(xl_string:format("<p>Bad request: ~p</p>", [Reason])), Req),
                                    {ok, Resp, 400}
                            end;
                        false -> 
                            process(Routes, Req, State, Module)
                    end
            end
    end;
process([], Req, State, _) ->
    {ok, Req1} = cowboy_req:reply(404, [], <<"">>, Req),
    {ok, Req1, State}.


prepare_response(Result, #route{out_format=raw}) ->
    {ok, Result};
prepare_response(Result, #route{output_spec=OutputSpec,produces={ContentType, Converter},out_format=auto})->
    Converter:to(Result, ContentType, OutputSpec);
prepare_response(Result, R = #route{produces=ContentType, out_format=auto})->
    prepare_response(Result, R#route{produces={ContentType, gaucho_default_converter}}).

extract_path_variables(Req, Route) ->
    case re:run(Route#route.raw_path, "{([^/:]*):?[^/]*}", [global, {capture, [1], list}]) of
	{match, VariableNames} ->
	    Keys = [list_to_atom(Name)||[Name] <- VariableNames],
	    {Path, _} = cowboy_req:path(Req),
        RawPath = xl_convert:to_string(Path),
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
    xl_lists:eforeach(fun({Name, Acceptors, TransportOpts}) ->
        cowboy:start_http(Name, Acceptors, TransportOpts,[{dispatch, Dispatch}])
    end, Listeners).




