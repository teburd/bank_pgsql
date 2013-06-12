# See LICENSE for licensing information.

PROJECT = bank_pgsql

# Options

PLT_APPS = crypto

# Dependencies

DEPS = bank
dep_bank = https://github.com/bfrog/bank.git transactions

include erlang.mk
