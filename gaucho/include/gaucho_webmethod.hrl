-record(webmethod, {
    path = erlang:error({required, path}) :: binary(),
    summary :: binary(),
    http_methods = [get] :: [atom()],
    produces = "text/plain" :: string(),
    result_format = auto :: auto | raw,
    param_spec = [],
    result_type = [],
    module :: module(),
    function :: atom(),
    raw_path :: binary()
}).

-record(webmethod_param, {
    name :: atom(),
    from :: header | path | body | 'query' | cookie | ip | request_uri,
    type,
    validators = []
}).
