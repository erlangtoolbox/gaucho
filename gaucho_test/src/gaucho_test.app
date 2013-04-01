{application, gaucho_test, [
    {description, "StrikeAd RTB/Coordinator"},
    {modules, []},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        crypto,
        public_key,
        ssl,
        cowboy
    ]},
    {mod, {gaucho_test_app, []}},
    {env, [
        {dispatch, [
            {'_', [
                {"/user/[...]", user_handler, []}
            ]}
        ]},
		{listeners, [
			{my_http_listener, 100, [{port, 8080}]}		]}
    ]}
]}.
