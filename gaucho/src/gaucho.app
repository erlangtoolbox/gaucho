{application, gaucho, [
    {description, "An extension for cowboy to simplify writing of RESTful services"},
    {vsn, "1"},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        xl_stdlib,
        cowboy
    ]},
    {env, [
        {callback, gaucho_callback}
    ]}
]}.
