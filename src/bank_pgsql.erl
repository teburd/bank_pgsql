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

-define(PGSQL_PROTOCOL_VERSION, <<196608:32>>).


%% @doc Connect to a postgresql database.
%% By default this will connect to a localhost running postgresql server using
%% trusted auth
%% @end
-spec connect(bank_client:opts()) -> {ok, bank_client:state()}
	| {fatal, opts | timeout | auth | inet:posix(), string()}.
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
	{ok, State}.


%% @private
%% @doc Connect to the database over a socket
connect_begin(State=#state{timeout=Timeout, host=Host, port=Port}) ->
	case gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, raw}], Timeout) of
		{ok, Socket} ->
			close_on_fatal(Socket, connect_send_auth(State#state{socket=Socket}));
		{error, Reason} ->
			Message = io_lib:format("Failed to connect to host ~p on port ~p",
				[Host, Port]),
			{fatal, Reason, Message}
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
			[Msg | ["database", 0, Database, 0]]
	end,
	Msg1 = [?PGSQL_PROTOCOL_VERSION, Msg0, 0],
	case send_msg(Socket, Timeout, Msg1) of
		ok ->
			connect_recv_auth(State);
		Other ->
			Other
	end.

%% @private
%% @doc Wait for authentication message reply, send authentication password
%% if needed and authentication method is supported (trust, md5)
connect_recv_auth(State=#state{socket=Socket, timeout=Timeout}) ->
	case recv_msg(Socket, Timeout) of
		{error, Info} ->
			Message = error_message(Info),
			{fatal, auth, Message};
		auth_ok ->
			{ok, State};
		%%{auth_md5, Hash} ->
		%%	connect_send_auth_md5(Hash, State);
		{AuthType, _} when is_atom(AuthType) ->
			Message = io_lib:format("Unsupported authentacation request ~p", [AuthType]),
			{fatal, auth, Message};
		AuthType when is_atom(AuthType) ->
			Message = io_lib:format("Unsupported authentacation request ~p", [AuthType]),
			{fatal, auth, Message}
	end.

%% @private
%% @doc Decode a message from PostgreSQL
decode_msg($E, Rest) ->
	{error, decode_error_fields(Rest)};
decode_msg($R, <<0:?int32>>) ->
	auth_ok;
decode_msg($R, <<2:?int32>>) ->
	auth_kerberos_v5;
decode_msg($R, <<3:?int32>>) ->
	auth_clear_text;
decode_msg($R, <<5:?int32, Salt/binary>>) ->
	{auth_md5, Salt};
decode_msg($R, <<8:?int32, 7:?int32>>) ->
	auth_gssapi;
decode_msg($R, <<8:?int32, 9:?int32>>) ->
	auth_sspi;
decode_msg($R, <<8:?int32, Rest/binary>>) ->
	{auth_gss_cont, Rest};
decode_msg(_, _) ->
	{fatal, decode, "!!!Unknown message!!!"}.

%% @private
%% @doc Decode error fields in to a proplist
decode_error_fields(ErrorFields) ->
	decode_error_fields(ErrorFields, []).

decode_error_fields(<<0>>, Acc) ->
	Acc;
decode_error_fields(<<Type:8, More/binary>>, Acc) ->
	Key = decode_error_name(Type),
	{Value, More0} = decode_string(More),
	case Key of 
		undefined ->
			decode_error_fields(More0, Acc);
		_ ->
			decode_error_fields(More0, [{Key, Value} | Acc])
	end.

%% @private
%% @doc Decode error type to atom
decode_error_name($S) ->
	severity;
decode_error_name($C) ->
	code;
decode_error_name($M) ->
	message;
decode_error_name($D) ->
	detail;
decode_error_name($H) ->
	hint;
decode_error_name($P) ->
	position;
decode_error_name($p) ->
	internal_position;
decode_error_name($q) ->
	internal_query;
decode_error_name($W) ->
	where_context;
decode_error_name($F) ->
	file;
decode_error_name($L) ->
	line;
decode_error_name($R) ->
	routine;
decode_error_name(_) ->
	undefined.

%% @private
%% @doc Decode a single null-terminated string
decode_string(Bin) ->
    decode_string(Bin, <<>>).

%% @private
decode_string(<<0, Rest/binary>>, Str) ->
    {Str, Rest};
decode_string(<<C, Rest/binary>>, Str) ->
    decode_string(Rest, <<Str/binary, C>>).


%% @private
%% @doc Helper to close the socket on a fatal error
close_on_fatal(Socket, {fatal, Reason, Message}) ->
	gen_tcp:close(Socket),
	{fatal, Reason, Message};
close_on_fatal(_, Result) ->
	Result.

%% @private
%% @doc Recieve and decode a single message from the socket with a timeout
recv_msg(Socket, Timeout) ->
	recv_msg_type(Socket, Timeout).

%% @private
%% @doc Receive the first 5 bytes which contains the type and len of the message
recv_msg_type(Socket, Timeout) ->
	case gen_tcp:recv(Socket, 5, Timeout) of
		{ok, <<Type:8, Len:32>>} ->
			recv_msg_body(Socket, Timeout, Type, Len);
		{error, timeout} ->
			{fatal, timeout, "Timed out recveiving message header"};
		{error, Reason} ->
			{fatal, Reason, "Error receiving message header"}
	end.

%% @private
%% @doc Receive the body of the message and then decode it
recv_msg_body(Socket, Timeout, Type, Len) ->
	Len2 = Len - 4,
	case gen_tcp:recv(Socket, Len2, Timeout) of
		{ok, Body} ->
			decode_msg(Type, Body);
		{error, timeout} ->
			{fatal, timeout, "Timed out recveiving message body"};
		{error, Reason} ->
			{fatal, Reason, "Error receiving message body"}
	end.

%% @private 
%% @doc Encode a message with a type and send it with a timeout
%%send_msg(Socket, Timeout, Type, Msg) ->
%%	Msg0 = iolist_to_binary(Msg),
%%	Data = <<Type:8, (byte_size(Msg0) + 4):?int32, Msg0/binary>>,
%%	send_data(Socket, Timeout, Data).

%% @private 
%% @doc Encode a message and send it with a timeout
send_msg(Socket, Timeout, Msg) ->
	Msg0 = iolist_to_binary(Msg),
	Data = <<(byte_size(Msg0) + 4):?int32, Msg0/binary>>,
	send_data(Socket, Timeout, Data).

%% @private 
%% @doc Send data on a socket with a timeout.
send_data(Socket, Timeout, Data) ->
	case inet:setopts(Socket, [{send_timeout, Timeout}, {send_timeout_close, true}]) of
		ok ->
			send_data(Socket, Data);
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

%% @private
%% @doc Generate a useful error message error response fields
error_message(Info) ->
	proplists:get_value(message, Info).
