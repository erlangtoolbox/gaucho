-module(gaucho).

-include("gaucho.hrl").
-include_lib("cowboy/include/http.hrl").
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
    {RawPath, _} = cowboy_http_req:raw_path(Req),
    case lists:last(Req#http_req.path) of
        <<"_api">> ->
            {ok, Req1} = cowboy_http_req:reply(200, [], gaucho_utils:get_api(AllRoutes), Req),
            {ok, Req1, 200};
        _ ->
            case re:run(RawPath, Route#route.path, [{capture, all, list}]) of
                nomatch ->
                    process(Routes, Req, State,  Module);
                {match, _} ->
                    Method = case cowboy_http_req:method(Req) of
                        {RawMethod, _} when is_binary(RawMethod) ->
                            binary_to_atom(RawMethod, utf8);
                        {RawMethod, _} when is_atom(RawMethod) ->
                            list_to_atom(string:to_lower(atom_to_list(RawMethod)))
                    end,
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
                                            {ok, Req1} = cowboy_http_req:reply(200, [{'Content-Type', ContentType}], Response, Req),
                                            {ok, Req1, 200};
                                        ok -> 
                                            {ok, Req, 204};
                                        {error, Status} when is_integer(Status) ->
                                            {ok, Req1} = cowboy_http_req:reply(Status, Req),
                                            {ok, Req1, Status};
                                        {error, {Status, Message}} -> 
                                            {ok, Req1} = cowboy_http_req:reply(Status, [], Message, Req),
                                            {ok, Req1, Status};

                                        {error, UnexpectedError} ->
                                            io:format("~p~n", [UnexpectedError]),
                                            {ok, Req1} = cowboy_http_req:reply(404, Req),
                                            {ok, Req1, 404};
                                        UnexpectedResult  ->
                                            Info = io_lib:format("~p~n", [UnexpectedResult]),
                                            {ok, Resp} = cowboy_http_req:reply(500, [], xl_convert:to_binary(Info), Req),
                                            {ok, Resp, 500}
                                    end;
                                {error, Reason} when is_list(Reason) ->
                                    {ok, Resp} = cowboy_http_req:reply(400, [], xl_convert:to_binary(Reason), Req),
                                    {ok, Resp, 400};
                                {error, Reason} ->
                                    {ok, Resp} = cowboy_http_req:reply(400, [], xl_convert:to_binary(xl_string:format("~p", [Reason])), Req),
                                    {ok, Resp, 400}
                            end;
                        false -> 
                            process(Routes, Req, State, Module)
                    end
            end
    end;
process([], Req, State, _) ->
    {ok, Req1} = cowboy_http_req:reply(404, [], <<"">>, Req),
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




