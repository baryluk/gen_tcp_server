-module(example_server_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Port) when is_integer(Port) ->
	Id_server_sup = list_to_atom("example_server_sup_" ++ integer_to_list(Port)),
	supervisor:start_link({local, Id_server_sup}, ?MODULE, Port).

init(Port) when is_integer(Port) ->
	Id_server_gen = list_to_atom("example_server_" ++ integer_to_list(Port)),
	Id_server_connection_sup = list_to_atom("example_server_connection_sup_" ++ integer_to_list(Port)),
	{ok, {{one_for_one, 1, 60},
		[{Id_server_gen, {example_server, start_link, [Port]},
			permanent, 20000, worker, [example_server]},
		{Id_server_connection_sup, {example_server_connection_sup, start_link, [Port]},
			permanent, infinity, supervisor, [example_server_connection_sup]}]}}.
