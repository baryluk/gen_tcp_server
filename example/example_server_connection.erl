-module(example_server_connection).
-author('baryluk@smp.if.uj.edu.pl').

-behaviour(gen_tcp_server_connection).

-export([start_link/5]).

-export([init/5]).
-export([handle_connection/3, handle_data/4, handle_timeout/4, handle_msg/4, handle_close/4]).
-export([code_change/2]).

-compile([export_all]).

-include("example_server_connection.hrl").


start_link(Port, ?MODULE, ClientSocket, ServerState, AdditionalState) when is_integer(Port) ->
	gen_tcp_server_connection:start_link(Port, ?MODULE, ClientSocket, ServerState, AdditionalState).

enable_buffering(ClientSocket) ->
	case prim_inet:setopts(ClientSocket, [{delay_send, true}, {sndbuf, 1024}]) of
		ok ->
			{ok, ClientSocket};
		Error ->
			gen_tcp:close(ClientSocket),
			Error
	end.

init(_Port, ?MODULE, ClientSocket, _ServerState, _AdditionalState) when is_integer(_Port) ->
	process_flag(trap_exit, true),
	%{ok, ClientSocket} = enable_buffering(ClientSocket),
	{ok, {HostAddr, Port}} = inet:peername(ClientSocket),
	{ok, {hostent, HostName, [], inet, _IPV46, _IPAddresseList}} = inet:gethostbyaddr(HostAddr),
	ClientState = #client_state{},
	io:format("New connection initialized: Client: ~p:~p, reverse dns: ~p~n", [HostAddr, Port, HostName]),
	{ok, ClientState}.

handle_connection(ClientState, _ClientSocket, _ServerState) ->
	io:format("Connection~n", []),
	{send, [<<"Banner: Hello">>,crlf], ClientState}.

process_input(FullLine) ->
	Len = size(FullLine) - 2, % chomp,
	<<Line:Len/binary, "\r\n">> = FullLine,
	{ping, Line}.

handle_data(ClientState=#client_state{}, _ClientSocket, _ServerState, Data) ->
	io:format("Data=~p~n", [Data]),
	ParsedInput = process_input(Data),
	io:format("Parsed data=~p~n", [ParsedInput]),

	case ParsedInput of
		{ping, <<"status">>} ->
			{ni, ClientState};  % not implemented
		{ping, <<"bye">>} ->
			IoList = ["Good bye!", crlf],
			NewClientState = ClientState,
			{send_close, IoList, quit, NewClientState};
		{ping, <<"time">>} ->
			IoList = [io_lib:format("~p~n", [erlang:now()]), crlf],
			NewClientState = ClientState,
			{send, IoList, NewClientState};
		{ping, Text} ->
			IoList = ["pong: ", Text, crlf],
			NewClientState = ClientState,
			{send, IoList, NewClientState};
		{error, ErrorMessage} ->
			{error, ClientState, {client_error, ErrorMessage}};
		{unknown, _Line, _} ->
			IoList = [<<"Unknown command">>],
			{send, IoList, ClientState};
		_ ->
			{ok, ClientState}
	end.

% badargs

%% Timeout

handle_timeout(ClientState=#client_state{}, _ClientSocket, _ServerState, _Timeout) ->
	IoList = [<<"Closing">>, crlf],
	{send_close, IoList, timeout, ClientState}.

%% Close

handle_close(_ClientState, _ClientSocket, _ServerState,
		{handler, {exception, error, socket_closed, _Stack}}) ->
	io:format("socket_closed~n"),
	{ok, socket_closed};
handle_close(_ClientState, _ClientSocket, _ServerState,
		{handler, {exception, Error, EValue, Stack}}) ->
	io:format("exception ~p:~p, call stack: ~p~n", [Error, EValue, Stack]),
	{ok, exception};
handle_close(_ClientState, _ClientSocket, _ServerState,
		{handler, Reason}) ->
	{ok, Reason};
handle_close(_ClientState, _ClientSocket, _ServerState,
		{handler, timeout_close, Reason}) ->
	{ok, Reason};
handle_close(_ClientState, _ClientSocket, _ServerState, Reason) ->
	{ok, Reason}.

%% Other messages


% handle_msg function


handle_msg(ClientState, _ClientSocket, _ServerState, {'EXIT', Pid, _Why}) ->
	io:format("exit message ~p, why ~p~n", [Pid, _Why]),
	{ok, ClientState};

handle_msg(ClientState, _ClientSocket, _ServerState, {ping4, Data}) ->
	{send, [Data], ClientState};

handle_msg(ClientState, _ClientSocket, _ServerState, Msg) ->
	io:format("unknown message ~p~n", [Msg]),
	{ok, ClientState}.


code_change(_OldVsn, ClientState) ->
	{ok, ClientState}.
