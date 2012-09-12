-module(gaucho_utils).
-include("gaucho.hrl").
-include_lib("cowboy/include/http.hrl").
-compile({parse_transform, do}).
-compile(export_all).

-spec get_attribute_types/1 :: (list()) -> tuple().
get_attribute_types([Spec | Specs]) ->
    [get_attribute_type(Spec)| get_attribute_types(Specs)];

get_attribute_types([]) -> [].


-spec get_attribute_type/1 :: (any()) -> tuple().
get_attribute_type({remote_type,_, [{atom,_, option_m},{atom,_,monad},[Type]]}) ->
    {maybe, get_attribute_type(Type)};

get_attribute_type({type, _, SimpleType, []}) when SimpleType =/= list->
    SimpleType;

get_attribute_type({type, _, record, [{atom, _, RecordName}]}) ->
    {record, RecordName};

get_attribute_type(UnsupportedType) ->
    erlang:error(unsupported_type, [UnsupportedType]).


%% process lists and error_m:monad type that are not allowed in attribute specs but allowed in output spec
-spec get_output_type/1 :: (any()) -> tuple().
get_output_type({remote_type, _, [{atom, _, error_m},{atom, _, monad},[Type]]}) ->
    get_output_type(Type);
get_output_type({type, _, list, [Type]}) ->
    {list, get_output_type(Type)};
get_output_type(Type) ->
    get_attribute_type(Type).

get_body(Req) ->
    case cowboy_http_req:body(Req) of 
        {ok, Body, Req1} ->
            {ok, {Body, Req1}};
        E -> E
    end.

-spec transform/2 :: (string()|list()|integer()|binary()|float(), atom()) -> any().
transform(Value, To) ->
    Function = list_to_atom(string:concat("to_", atom_to_list(To))),
    apply(xl_convert, Function, [Value]).
-spec get_attributes/3 :: (#http_req{}, list(), list()) -> error_m:monad(any()).
get_attributes(Req, PathVariables, Attributes) ->
        get_attributes(Req, PathVariables, [], Attributes).

get_attributes(Req, PathVariables, Acc, 
        [{#param{name=Name, from={body, {ContentType, Converter}}, validators=Validators}, AttrType}| Attributes]) ->

        do([error_m ||
            {Body, Req1} <- get_body(Req),
            Content <- Converter:from(Body, ContentType, AttrType),
            get_attributes(Req1, PathVariables, [{Name, Content} | Acc], Attributes)
        ]);

get_attributes(Req, PathVariables, Acc, 
        [{#param{name=Name, from={body, ContentType}, validators=Validators}, Spec}| Attributes]) ->
        do([error_m ||
            {Body, Req1} <- get_body(Req),
            Content <- gaucho_default_converter:from(Body,ContentType, Spec),
            get_attributes(Req1, PathVariables, [{Name, Content} | Acc], Attributes)
        ]);
get_attributes(Req, PathVariables, Acc, [{#param{name=Name, from=Spec, validators=Validators}, AttributeType}| Attributes]) when is_atom(AttributeType) ->
    do([error_m ||
            {Val, Req1} <- case Spec of
                path ->
                    case xl_lists:kvfind(Name, PathVariables) of
                        {ok, Value} -> {ok, {Value, Req}};
                        undefined -> {error, {unknown_pathvariable, Name}}
                    end;
                'query' ->
                    {ok, cowboy_http_req:qs_val(atom_to_binary(Name, utf8), Req)};

                cookie ->
                    {ok, cowboy_http_req:cookie(atom_to_binary(Name, utf8), Req)};

                header ->
                    {ok, cowboy_http_req:header(Name, Req)};
                _ -> {error, {unknown_spec, Spec}}

            end,
            CValue <- return(transform(Val, AttributeType)),
            get_attributes(Req, PathVariables, [{Name, CValue}| Acc], Attributes)
    ]);

get_attributes(_, _, Acc, _) ->
    {ok, lists:reverse(Acc)}.

get_api(Routes) ->
    {ok, Api} = get_api(Routes, ""),
    xl_convert:to_binary(Api).

get_api([#route{accepted_methods=[Method], raw_path=RawPath, output_spec=OutSpec, attribute_specs=InSpec}| Routes], Acc) ->
    do([error_m||
            ApiString <- return(io_lib:format("~s ~s~n\tInputSpec: ~p~n\tOutputSpec: ~p ~n~n", [xl_string:to_upper(xl_convert:to_string(Method)), RawPath, InSpec, OutSpec])),
            %ApiString <- return(io_lib:format("~s ~s~n~n", [xl_string:to_upper(xl_convert:to_string(Method)), RawPath])),
            get_api(Routes, Acc ++ ApiString)
        ]);
get_api([], Acc) ->
    {ok, Acc}.
