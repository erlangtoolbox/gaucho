-module(gaucho).

-compile({parse_transform, do}).

-include("gaucho_webmethod.hrl").

-export([process/3, parse_transform/2, start/2, generate_api/1]).

parse_transform(Forms, Options) ->
    gaucho_pt:parse_transform(Forms, Options).

process(WebMethods, Request, State) ->
    Callback = xl_application:get_env(gaucho, callback, gaucho_callback),
    Body = case cowboy_req:body(Request) of
        {ok, B, _} -> B;
        {error, Reason} -> Reason
    end,
    {Url, _} = cowboy_req:url(Request),
    Callback:request(Url, Body),
    case perform(Request, WebMethods) of
        {ok, Status} when is_integer(Status) ->
            {ok, Resp} = cowboy_req:reply(Status, Request),
            {ok, Resp, State};
        {ok, {302, Location}} ->
            {ok, Resp} = cowboy_req:reply(302, [{<<"Location">>, xl_convert:to_binary(Location)}], Request),
            {ok, Resp, State};
        {ok, {Status, ContentType, Content}} ->
            {ok, Resp} = cowboy_req:reply(Status, [{<<"Content-Type">>, ContentType}], Content, Request),
            {ok, Resp, State};
        {error, Status} when is_integer(Status) ->
            {ok, Resp} = cowboy_req:reply(Status, Request),
            Callback:error(Status, <<"">>, Url, Body),
            {ok, Resp, State};
        {error, {400, Content}} when is_list(Content) ->
            {ok, Resp} = cowboy_req:reply(400, [], xl_string:join(Content, <<"\n">>), Request),
            Callback:error(400, Content, Url, Body),
            {ok, Resp, State};
        {error, {Status, Content}} ->
            {ok, Resp} = cowboy_req:reply(Status, [], xl_string:join(Content, <<"\n">>), Request),
            Callback:error(Status, Content, Url, Body),
            {ok, Resp, State}
    end.

perform(Request, WebMethods) ->
    {Path, _} = cowboy_req:path(Request),
    {HttpMethod, _} = gaucho_cowboy:http_method(Request),
    case gaucho_webmethod:find_webmethod(Path, HttpMethod, WebMethods) of
        {ok, WebMethod = #webmethod{module = Module, function = Function}} ->
            case gaucho_cowboy:build_arguments(Request, WebMethod) of
                {ok, Arguments} ->
                    case apply(Module, Function, Arguments) of
                        {ok, {302, _Location}} = Redirect->
                            Redirect;
                        {ok, Content} ->
                            ContentType = gaucho_webmethod:content_type(WebMethod),
                            {ok, Response} = prepare_response(Content, WebMethod),
                            {ok, {200, ContentType, Response}};
                        ok -> {ok, 204};
                        E = {error, Status} when is_integer(Status) -> E;
                        E = {error, {Status, _}} when is_integer(Status) -> E;
                        {error, UnexpectedError} -> {error, {500, UnexpectedError}};
                        UnexpectedResult -> {error, {500, UnexpectedResult}}
                    end;
                {error, Reason} ->
                    {error, {400, Reason}}
            end;
        undefined -> {error, 404}
    end.

prepare_response(Result, #webmethod{result_format = raw}) ->
    {ok, Result};
prepare_response(Result, #webmethod{result_type = ResultType, produces = {ContentType, Converter}, result_format = auto}) ->
    Converter:to(Result, ContentType, ResultType);
prepare_response(Result, R = #webmethod{produces = ContentType, result_format = auto}) ->
    prepare_response(Result, R#webmethod{produces = {ContentType, gaucho_default_converter}}).

-spec start/2 :: (term(), [{atom(), pos_integer(), atom(), [term()], atom(), [term()]}]) -> error_m:monad(ok).
start(Dispatch, Listeners) ->
    xl_lists:eforeach(fun({Name, Acceptors, TransportOpts}) ->
        cowboy:start_http(Name, Acceptors, TransportOpts, [{dispatch, Dispatch}])
    end, Listeners).

generate_api(Mapping) ->
    Calls = lists:map(fun(#webmethod{http_methods = Methods, raw_path = RawPath, param_spec = ParamSpec, result_type = ResultType}) ->
        xl_string:format("~s ~p~n\tParams: ~p~n\tOutputSpec: ~p ~n", [RawPath, Methods, ParamSpec, ResultType])
    end, Mapping),
    {ok, xl_string:join(Calls, <<"\n">>)}.
