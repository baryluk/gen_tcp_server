-module(example_app).
-behaviour(application).

-export([start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
	{ok, Pid} = example_sup:start_link([9990,9991]),
	{ok, Pid}.

stop(_State) ->
	ok.

config_change(_Changed, _New, _Removed) ->
	ok.
