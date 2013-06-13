# See LICENSE for licensing information.

PROJECT = bank_pgsql

# Tests

CT_SUITES = bank_pgsql

# Options

PLT_APPS = crypto

# Dependencies

DEPS = bank
dep_bank = https://github.com/bfrog/bank.git transactions


create_db:
	psql -f test/create.sql

drop_db:
	psql -f test/drop.sql

include erlang.mk
