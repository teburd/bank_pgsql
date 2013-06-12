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
	socket = undefined :: undefined
}).

%% @doc Connect to a postgresql database.
%% By default this will connect to a localhost running postgresql server using
%% trusted auth
%% @end
-spec connect(bank_client:opts()) -> {ok, bank_client:state()}
	| {error, timeout | auth | network, string()}.
connect(_Opts) ->
	{ok, #state{}}.

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
