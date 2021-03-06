-module(gaucho_pt).

-export([parse_transform/2]).

-include("gaucho_webmethod.hrl").


parse_transform(Forms, _Options) ->
    {eof, Line} = lists:keyfind(eof, 1, Forms),
    Mapping = erl_parse:abstract([swagger_webmethod(gaucho_webmethod:module(Forms))|[api_webmethod(gaucho_webmethod:module(Forms)) | gaucho_webmethod:mapping(Forms)]]),
    Forms1 = lists:keydelete(eof, 1, Forms),
    Forms2 = lists:append(Forms1, [
        cowboy_handle_ast(Mapping),
        cowboy_init_ast(),
        cowboy_terminate_ast(),
        api_ast(Mapping),
        swagger_ast(Mapping),
        {eof, Line}
    ]),
    FirstFun = lists:keyfind(function, 1, Forms2),
    Export = {attribute, 0, export, [{init, 3}, {handle, 2}, {terminate, 3}, {'_api', 0}, {'_swagger', 2}]},
    FormsWithExport = xl_lists:insert_before(FirstFun, Export, Forms2),
    [Form || Form <- FormsWithExport, element(3, Form) =/= webmethod].

cowboy_init_ast() ->
    {function, 0, init, 3, [
        {clause, 0, [
            {tuple, 0, [{var, 0, '_Any'}, {atom, 0, http}]}, {var, 0, 'Req'}, {var, 0, 'State'}
        ], [], [
            {tuple, 0, [{atom, 0, ok}, {var, 0, 'Req'}, {var, 0, 'State'}]}
        ]}
    ]}.

cowboy_terminate_ast() ->
    {function, 0, terminate, 3, [
        {clause, 0, [
            {var, 0, '_Reason'},
            {var, 0, '_Req'},
            {var, 0, '_State'}
        ], [], [{atom, 0, ok}]}
    ]}.

cowboy_handle_ast(Mapping) ->
    {function, 0, handle, 2, [
        {clause, 0, [{var, 0, 'Req'}, {var, 0, 'State'}],
            [], [
            {match, 0, {var, 0, 'Mapping'}, Mapping},
            {call, 0, {remote, 0, {atom, 0, gaucho}, {atom, 0, process}}, [
                {var, 0, 'Mapping'},
                {var, 0, 'Req'},
                {var, 0, 'State'}
            ]}
        ]}
    ]}.

api_ast(Mapping) ->
    {function, 0, '_api', 0, [
        {clause, 0, [],
            [], [
            {call, 0, {remote, 0, {atom, 0, gaucho}, {atom, 0, generate_api}}, [
                Mapping
            ]}
        ]}
    ]}.

api_webmethod(Module) ->
    #webmethod{
        path = "/_api",
        http_methods = [get],
        produces = "text/plain",
        result_format = auto,
        param_spec = [],
        result_type = string,
        module = Module,
        function = '_api',
        raw_path = "/_api"
    }.

swagger_ast(Mapping) ->
    {function, 0, '_swagger', 2, [
            {clause, 0, [{var, 0, 'Host'},{var, 0, 'Port'}],
            [], [
            {call, 0, {remote, 0, {atom, 0, gaucho}, {atom, 0, generate_swagger_api}}, [
                Mapping,
                {var, 0, 'Host'},
                {var, 0, 'Port'}
            ]}
        ]}
    ]}.

swagger_webmethod(Module) ->
    #webmethod{
        path = "/_swagger",
        http_methods = [get],
        produces = {"application/json", swagger_converter},
        result_format = auto,
        param_spec = [{webmethod_param,host,host,binary,[]},{webmethod_param,port,port,binary,[]}],
        result_type = string,
        module = Module,
        function = '_swagger',
        raw_path = "/_swagger"
    }.
