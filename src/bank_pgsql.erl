%% Copyright (c) 2013, Tom Burdick <thomas.burdick@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc PostgreSQL driver for Bank.
-module(bank_pgsql).

%% API.
-export([connect/1]).
-export([close/1]).
-export([ping/1]).
-export([prepare/4]).
-export([unprepare/3]).
-export([execute/4]).
-export([query/4]).
-export([begin_transaction/1]).
-export([commit_transaction/1]).
-export([rollback_transaction/1]).

-behaviour(bank_client).

-record(state, {
	timeout = undefined :: non_neg_integer(),
	host = undefined :: string(),
	port = undefined :: integer(),
	user = undefined :: string(),
	password = undefined ::string(),
	database = undefined :: string(),
	socket = undefined :: undefined
}).

-define(int16, 1/big-signed-unit:16).
-define(int32, 1/big-signed-unit:32).


%% @doc Connect to a postgresql database.
%% By default this will connect to a localhost running postgresql server using
%% trusted auth
%% @end
-spec connect(bank_client:opts()) -> {ok, bank_client:state()}
	| {error, opts | timeout | auth | inet:posix(), string()}.
connect(Opts) ->
	case parse_opts(Opts) of
		{ok, State} ->
			connect_begin(State);
		Other ->
			Other
	end.


%% @doc Close connection to postgresql server if its connected.
-spec close(bank_client:state()) -> {ok, bank_client:state()}.
close(State) ->
	{ok, State}.

%% @doc Ping remote server to ensure connection is still alive
-spec ping(bank_client:state()) -> {boolean(), bank_client:state()}.
ping(State) ->
	{true, State}.

%% @doc Create a prepared statement for later use
-spec prepare(any(), string(), non_neg_integer(), bank_client:state()) ->
	bank_client:prepare_result() | bank_client:error_result().
prepare(_Stmt, _Query, _Timeout, State) ->
	{ok, State}.

%% @doc Destroy a prepared statement. 
-spec unprepare(any(), non_neg_integer(), bank_client:state()) ->
	bank_client:prepare_result() | bank_client:error_result().
unprepare(_Stmt, _Timeout, State) ->
	{ok, State}.

%% @doc Execute a prepared statement. 
-spec execute(any(), list(), non_neg_integer(), bank_client:state()) -> 
	bank_client:query_result() | bank_client:error_result().
execute(_Stmt, _Params, _Timeout, State) ->
	{rows, [], State}.

%% @doc Perform an ad-hoc query
%% This may create a temporary prepared statement to bind parameters to if any
%% are given.
%% @end
-spec query(any(), list(), non_neg_integer(), bank_client:state()) -> 
	bank_client:query_result() | bank_client:error_result().
query(_Query, _Params, _Timeout, State) ->
	{rows, [], State}.

%% @doc Begin a multi query transaction
-spec begin_transaction(bank_client:state()) ->
	bank_client:transaction_result() | bank_client:error_result().
begin_transaction(State) ->
	{ok, State}.

%% @doc Commit a multi query transaction
-spec commit_transaction(bank_client:state()) ->
	bank_client:transaction_result() | bank_client:error_result().
commit_transaction(State) ->
	{ok, State}.

%% @doc Rollback a multi query transaction
-spec rollback_transaction(bank_client:state()) -> bank_client:transaction_result() |
	bank_client:error_result().
rollback_transaction(State) ->
	{ok, State}.



%% Connection Steps

%% @private
%% @doc Parse the options proplist and fill out the state record. This
%% checks for option errors in a reasonable way.
%% @end
parse_opts(Opts) ->
	State = #state {
		timeout = proplists:get_value(timeout, Opts, 5000),
		host = proplists:get_value(host, Opts, "localhost"),
		port = proplists:get_value(port, Opts, 5432),
		user = proplists:get_value(user, Opts),
		password = proplists:get_value(password, Opts),
		database = proplists:get_value(database, Opts)
	},
	validate_user_opt(State).

%% @private
%% @doc Validate the user option exists
validate_user_opt(#state{user=undefined}) ->
	{error, opts, "Missing user connection option"};
validate_user_opt(State) ->
	State.


%% @private
%% @doc Connect to the database over a socket
connect_begin(State=#state{timeout=Timeout, host=Host, port=Port}) ->
	case gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, raw}], Timeout) of
		{ok, Socket} ->
			connect_send_auth(State#state{socket=Socket});
		_ ->
			Message = io_lib:format("Failed to connect to host ~p on port ~p",
				[Host, Port]),
			{error, network, Message}
	end.

%% @private
%% @doc Send the authentication message
connect_send_auth(State=#state{socket=Socket, user=User, database=Database, timeout=Timeout}) ->
	Msg = ["user", 0, User, 0],
	Msg0 = case Database of
		"" ->
			Msg;
		undefined ->
			Msg;
		_ ->
			Msg ++ ["database", 0, Database, 0]
	end,
	case send_msg(Socket, Timeout, 0, Msg0) of
		ok ->
			connect_recv_auth(State);
		Other ->
			Other
	end.

%% @private
%% @doc Wait for authentication message reply, send authentication password
%% if needed and authentication method is supported (trust, md5)
connect_recv_auth(State) ->
	{ok, State}.

%% @private 
%% @doc Encode a message with a type and send it with a timeout
send_msg(Socket, Timeout, Type, Msg) ->
	Msg0 = iolist_to_binary(Msg),
	Msg1 = <<Type:8, (byte_size(Msg0) + 4):?int32, Msg0/binary>>,
	send_msg(Socket, Timeout, Msg1).

%% @private 
%% @doc Send a msg on a socket with a timeout.
send_msg(Socket, Timeout, Msg) ->
	case inet:setopts(Socket, [{send_timeout, Timeout}, {send_timeout_close, true}]) of
		ok ->
			send_data(Socket, Msg);
		{error, Reason} ->
			{fatal, Reason, "Failed to set socket timeout options"}
	end.

%% @private
%% @doc Send data out on a socket
send_data(Socket, Data) ->
	case gen_tcp:send(Socket, Data) of
		ok ->
			ok;
		{error, timeout} ->
			{fatal, timeout, "Socket send timed out"};
		{error, Reason} ->
			{error, Reason, "Socket send failed"}
	end.

