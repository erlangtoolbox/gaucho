%% Copyright
-module(gaucho_webmethod).
-author("volodymyr.kyrychenko@strikead.com").

-include("gaucho_webmethod.hrl").

%% API
-export([mapping/1, compile_path/1, typeof/1, result_type/1, module/1, find_webmethod/3, content_type/1]).

mapping(Forms) ->
    Module = module(Forms),
    mapping([], Module, Forms).

mapping(Acc, _Module, []) -> lists:reverse(Acc);
mapping(Acc, Module, [WMSpec = {attribute, _, webmethod, {Path, HttpMethods, Produces, ResultFormat, Params}} | Forms]) ->
    case xl_lists:find(fun(A) -> element(3, A) == spec end, Forms) of
        {ok, {attribute, _, spec, {{Function, _}, [{type, _, 'fun', [{type, _, product, ParamTypes}, ResultSpec]}]}}} ->
            WM = web_method(Path, HttpMethods, Produces, lists:zip(Params, ParamTypes), ResultFormat, ResultSpec, Module, Function),
            mapping([WM | Acc], Module, Forms);
        undefined -> error({no_type_spec_for, WMSpec})
    end;
mapping(Acc, Module, [_ | Forms]) -> mapping(Acc, Module, Forms).

web_method(Path, HttpMethods, Produces, Params, ResultFormat, ResultSpec, Module, Function) ->
    #webmethod{
        path = compile_path(Path),
        http_methods = HttpMethods,
        produces = Produces,
        result_format = ResultFormat,
        param_spec = lists:map(fun(P) -> param_spec(P) end, Params),
        result_type = result_type(ResultSpec),
        module = Module,
        function = Function,
        raw_path = xl_convert:to(binary, Path)
    }.

param_spec({{Name, From}, TypeSpec}) -> param_spec({{Name, From, []}, TypeSpec});
param_spec({{Name, From, Validators}, TypeSpec}) ->
    #webmethod_param{
        name = Name,
        from = From,
        type = typeof(TypeSpec),
        validators = Validators
    }.

compile_path(Path) ->
    Compiled = case re:run(Path, "{([^/:]*):?([^/]*)}", [global, {capture, all, list}]) of
        {match, Replacements} ->
            lists:foldl(fun
                ([Macro, _, []], P) -> xl_string:replace(P, Macro, "([^/]+)");
                ([Macro, _, RegEx], P) -> xl_string:replace(P, Macro, RegEx)
            end, Path, Replacements);
        nomatch -> Path
    end,
    xl_string:join([Compiled, "$"], <<"">>).

typeof({remote_type, _, [{atom, _, option_m}, {atom, _, monad}, [Type]]}) -> {option, typeof(Type)};
typeof({type, _, Type, []}) when Type /= list -> Type;
typeof({type, _, record, [{atom, _, Name}]}) -> {record, Name};
typeof(UnsupportedType) -> error(unsupported_type, [UnsupportedType]).

result_type({remote_type, _, [{atom, _, error_m}, {atom, _, monad}, [Type]]}) -> result_type(Type);
result_type({type, _, list, [Type]}) -> {list, result_type(Type)};
result_type(Type) -> typeof(Type).

module(Forms) ->
    case xl_lists:find(fun(Form) -> element(3, Form) == module end, Forms) of
        {ok, Form} -> element(4, Form);
        undefined -> error(module_declaration_not_found)
    end.

find_webmethod(Path, HttpMethod, WebMethods) ->
    xl_lists:find(fun(#webmethod{http_methods = WMHttpMethods, path = WMPath}) ->
        lists:member(HttpMethod, WMHttpMethods) andalso re:run(Path, WMPath, [{capture, all, list}]) /= nomatch
    end, WebMethods).

content_type(#webmethod{produces = {ContentType, _}}) -> ContentType;
content_type(#webmethod{produces = ContentType}) -> ContentType.
