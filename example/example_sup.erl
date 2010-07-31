-module(example_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Ports) when is_list(Ports) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Ports).

init(Ports) when is_list(Ports) ->
	{ok, {{one_for_one, 1, 60},
		lists:map(fun (Port) -> 
				Id = list_to_atom("example_server_sup_" ++ integer_to_list(Port)),
				{Id, {example_server_sup, start_link, [Port]},
					permanent, infinity, supervisor, [example_server_sup]}
			end,
			Ports)
	}}.
