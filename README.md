Bank PostgreSQL Driver
=================

PostgreSQL driver for Bank.

Warning
-------

This project is currently in very early alpha and lacks features expected
from normal PostgreSQL usage.

Getting Started
---------------

Please see the [Bank README](https://github.com/extend/bank).


Usage
-----

This PostgreSQL client library uses near direct blocking socket calls. This
makes the client library very simple and efficient however it does require
keeping the state of the client around and incrementally updating it after
each call.

Connecting to a postgresql server takes as little as

```erlang
{ok, Client} = bank_pgsql:connect([{user, "dbuser"}]).
```

The client may then be used to perform queries

```erlang
{ok, Columns, Rows, Client2} = bank_pgsql:query("select now()", [], 1000, Client).
```

Errors are reported either as recoverable or fatal to the client connection.

The error messages are always in the form of

{error | fatal, Type::error_result_type(), Message::string()}

```erlang
{fatal, timeout, Message} = bank_pgsql:query("select * from bigtable", [], 1, Client).
```

Fatal errors mark that the client is no longer useful and the socket has been
closed.


Support
-------

 *  Official IRC Channel: #ninenines on irc.freenode.net
 *  [Mailing Lists](http://lists.ninenines.eu)
 *  [Commercial Support](http://ninenines.eu/support)
