%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2013 Tom Burdick
%% @doc Bank Postgresql Test Suite

-module(bank_pgsql_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% tests
-export([connect_close/1]).
-export([connect_bad_user/1]).

all() ->
	[
		connect_close,
		connect_bad_user
	].

init_per_suite(Config) ->
	Config.

end_per_suite(Config) ->
	Config.

connect_close(_Config) ->
	{ok, Client} = bank_pgsql:connect([{user, "bank_pgsql"}]),
	{ok, _Client0} = bank_pgsql:close(Client).

connect_bad_user(_Config) ->
	{fatal, Reason, Message} = bank_pgsql:connect([{user, "superdupermarioman"}]),
	ct:pal("bad user reason ~p, message ~s~n", [Reason, Message]).
