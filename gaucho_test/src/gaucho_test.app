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
                {[<<"user">>], user_handler, []},
                {[<<"user">>, '...'], user_handler, []}
            ]}
        ]},
		{listeners, [
			{http_listener, 100, cowboy_tcp_transport, [{port, 8080}], cowboy_http_protocol, []},
			{https_listener, 100, cowboy_ssl_transport, [
				{port, 8443}, 
				{certfile, "priv/ssl/cert.pem"},
	            {keyfile, "priv/ssl/key.pem"}, 
				{password, "cowboy"}
			], cowboy_http_protocol, []}
		]}
    ]}
]}.
