-record(route, { 
        path = erlang:error({required, path}), 
        accepted_methods = [get],
        produces = "text/plain", 
        out_format = auto,
        attribute_specs = [],
        output_spec = [],
        handler = undefined,
        raw_path = ""
    }).
-record(param, {
        name,
        from,
        validators = []
    }).
