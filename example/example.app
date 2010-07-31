{application, example,
	[{description, "Example of TCP server"},
	{vsn, "0.0.0.1"},
	{modules, [example_app, example_sup,
		example_server_sup, example_server_gen,
		example_server_connection_sup, example_server_connection_gen,
		gen_tcp_server, gen_tcp_server_connection
	]},
	{registered, [example]},
	{applications, [kernel, stdlib, sasl]},
	{mod, {example_app, []}},
	{env, [{listen_at_ports, [6667, 6668, 7776]}, {logfile, "example.log"}]}
	]
}.
