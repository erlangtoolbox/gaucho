-module(gaucho).

-compile({parse_transform, do}).

-include("gaucho_webmethod.hrl").
-include("swagger.hrl").

-export([process/3, parse_transform/2, start/2, generate_api/1, generate_swagger_api/1]).

parse_transform(Forms, Options) ->
    gaucho_pt:parse_transform(Forms, Options).

process(WebMethods, Request, State) ->
    Callback = xl_application:get_env(gaucho, callback, gaucho_callback),
    {Body, UpdatedRequest} = case cowboy_req:body(Request) of
        {ok, B, Request1} -> {B, Request1};
        {error, Reason} -> {Reason, Request}
    end,
    {Url, _} = cowboy_req:url(UpdatedRequest),
    Callback:request(Url, Body),
    case perform(UpdatedRequest, Body, State, WebMethods) of
        {ok, Status} when is_integer(Status) ->
            {ok, Resp} = cowboy_req:reply(Status, UpdatedRequest),
            {ok, Resp, State};
        {ok, {302, Location}} ->
            {ok, Resp} = cowboy_req:reply(302, [{<<"Location">>, xl_convert:to(binary, Location)}], UpdatedRequest),
            {ok, Resp, State};
        {ok, {Status, ContentType, Content}} ->
            
            {ok, Resp} = cowboy_req:reply(Status, [{<<"Content-Type">>, ContentType}], Content, UpdatedRequest),
            {ok, Resp, State};
        {error, Status} when is_integer(Status) ->
            {ok, Resp} = cowboy_req:reply(Status, UpdatedRequest),
            Callback:error(Status, <<"">>, Url, Body),
            {ok, Resp, State};
        {error, {Status, Content}} ->
            {ok, Resp} = cowboy_req:reply(Status, [], xl_convert:to(binary, xl_string:format("~p", [Content])), UpdatedRequest),
            Callback:error(Status, Content, Url, Body),
            {ok, Resp, State}
    end.

perform(Request, Body, State, WebMethods) -> {Path, _} = cowboy_req:path(Request),
    {HttpMethod, _} = gaucho_cowboy:http_method(Request),
    case gaucho_webmethod:find_webmethod(Path, HttpMethod, WebMethods) of
        {ok, WebMethod = #webmethod{module = Module, function = Function}} ->
            case gaucho_cowboy:build_arguments(Request, Body, State, WebMethod) of
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

-spec start/2 :: (term(), [{atom(), pos_integer(), atom(), [term()], atom(), [term()]}]) -> error_m:monad(ok).  start(Disp, Listeners) ->
    Dispatch = cowboy_router:compile(Disp),
    xl_lists:eforeach(fun({Name, Acceptors, TransportOpts}) ->
                cowboy:start_http(Name, Acceptors, TransportOpts, [{env, [{dispatch, Dispatch}]}])
    end, Listeners).

generate_api(Mapping) ->
    Calls = lists:map(fun(#webmethod{http_methods = Methods, raw_path = RawPath, param_spec = ParamSpec, result_type = ResultType}) ->
        xl_string:format("~s ~p~n\tParams: ~p~n\tOutputSpec: ~p ~n", [RawPath, Methods, ParamSpec, ResultType])
    end, Mapping),
    {ok, xl_string:join(Calls, <<"\n">>)}.

generate_swagger_parameters(GauchoParamsSpec) ->
    lists:foldl(
        fun(#webmethod_param{name = Name, from = From, type = Type}, Acc) -> 
                Required = case Type of
                    {option, _Type} -> false;
                    _Type -> true
                end,
                ParamType = case From of
                    {Val, _Spec} -> Val;
                    Val -> Val
                end,
                case lists:member(ParamType, [path, 'query', body, header]) of
                    true ->
                        [#parameter{name = Name, paramType = ParamType, required = Required}| Acc];
                    false -> Acc
                end
        end, [], GauchoParamsSpec).

generate_swagger_api(Mapping) ->
    Res = lists:foldl(fun(#webmethod{raw_path = RawPath_, http_methods = [Method], param_spec = ParamsSpec} = _WebMethod, Acc)-> 

                RawPath = xl_convert:to(binary, RawPath_), 
                case lists:keyfind(RawPath, 2, Acc) of
                    false ->
                        [#api{path = RawPath, operations = [#operation{method = xl_string:to_upper(xl_convert:to(binary, Method)), parameters = generate_swagger_parameters(ParamsSpec)}]}| Acc];
                    Found ->
                        NewAapi = Found#api{operations = [#operation{method = xl_string:to_upper(xl_convert:to(binary, Method)),
                                    parameters = generate_swagger_parameters(ParamsSpec)
                                }|Found#api.operations]},
                        lists:keyreplace(RawPath, 2, Acc, NewAapi)
                end
        end, [], Mapping),


    {ok, #swagger{apiVersion = <<"1.0">>,
            apis = Res}}.
    % Calls = lists:map(fun(#webmethod{http_methods = Methods, raw_path = RawPath, param_spec = ParamSpec, result_type = ResultType}) ->
        % xl_string:format("~s ~p~n\tParams: ~p~n\tOutputSpec: ~p ~n", [RawPath, Methods, ParamSpec, ResultType])
    % end, Mapping),
    % {ok, xl_string:join(Calls, <<"\n">>)}.
