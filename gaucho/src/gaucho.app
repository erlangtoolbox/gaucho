{application, gaucho, [
    {description, "An extension for cowboy to simplify writing of RESTful services"},
    {vsn, "1"},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        jiffy,
        xl_stdlib,
        xl_json,
        cowboy
    ]},
    {env, [
        {callback, gaucho_callback}
    ]}
]}.
