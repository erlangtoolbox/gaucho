%% Copyright
-module(gaucho_cowboy).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

-include("gaucho_webmethod.hrl").

%% API
-export([http_method/1, path_bindings/3, build_arguments/4]).

http_method(Request) ->
    {RawMethod, Request2} = cowboy_req:method(Request),
    {xl_convert:to(atom, xl_string:to_lower(RawMethod)), Request2}.

path_bindings(RegExpPath, RawPath, Request) ->
    {Path, _} = cowboy_req:path(Request),
    case re:run(RawPath, "{([^/:]*):?[^/]*}", [global, {capture, [1], list}]) of
        {match, VariableNames} ->
            Keys = [list_to_atom(Name) || [Name] <- VariableNames],
            {match, [_ | Values]} = re:run(Path, RegExpPath, [{capture, all, list}]),
            lists:zip(Keys, Values);
        nomatch -> []
    end.

build_arguments(Request, Body, State, #webmethod{path = Path, raw_path = RawPath, param_spec = Params}) ->
    Bindings = path_bindings(Path, RawPath, Request),
    do([error_m ||
        {Arguments, _} <- xl_lists:efoldl(
            fun(#webmethod_param{name = Name, from = From, type = Type, validators = Validators}, {Values, R}) ->
                {Source, {ContentType, Converter}} = case From of
                    S when is_atom(S) -> {S, {"x-erlang/unknown-value", gaucho_default_converter}};
                    {S, C} when is_atom(C) -> {S, {"x-erlang/unknown-value", C}};
                    X -> X
                end,
                do([error_m ||
                    {Value, Request2} <- value(Bindings, R, Body, State, Name, Source),
                    Converted <- Converter:from(Value, ContentType, Type),
                    xl_lists:eforeach(fun(V) -> validate(Converted, V) end, Validators),
                    return({[Converted | Values], Request2})
                ])
            end, {[], Request}, Params),
        return(lists:reverse(Arguments))
    ]).

value(_, Request, _, _, _, request_uri) -> {ok, cowboy_req:url(Request)};
value(_, Request, _, _, _, query_string) -> {ok, gaucho_req:qs_lowercase(Request)};
value(_, Request, _, _, _, query_values) -> {ok, gaucho_req:qs_vals(Request)};
value(_, Request, _, _, _, ip) ->
    {IpAddr, Request2} = cowboy_req:peer(Request),
    {ok, {xl_string:join(tuple_to_list(IpAddr), <<".">>), Request2}};
value(Bindings, Request, _, _State, Name, path) -> 
    case xl_lists:kvfind(Name, Bindings) of
        undefined -> {ok, {undefined, Request}};
        {ok, Value} -> {ok, {Value, Request}}
    end;
value(_Bindings, Request, _Body, State, Name, state) ->
    case xl_lists:kvfind(Name, State) of
        undefined -> {ok, {undefined, Request}};
        {ok, Value} -> {ok, {Value, Request}}
    end;
value(_Bindings, Request, _, _State, Name, 'query') -> {ok, gaucho_req:qs_val_ignore_case(Name, Request)};
value(_Bindings, Request, _, _State, Name, cookie) -> {ok, gaucho_req:cookie_ignore_case(Name, Request)};
value(_Bindings, Request, _, _State, Name, header) -> {ok, cowboy_req:header(xl_convert:to(binary, Name), Request)};
value(_Bindings, Request, Body, _State, _Name, body) -> {ok, {Body, Request}}.

validate(Value, {Module, Method}) ->
    validate(Value, {Module, Method, []});
validate(Value, {Module, Method, Attributes}) ->
    apply(Module, Method, [Value | Attributes]).
