PKG_NAME = rubato
APP_NAME = rubato

SRC_DIR = src
EBIN_DIR = ebin
INCLUDE_DIR = include

MOCHIWEB_EBIN = ../mochiweb/ebin
EVO_EBIN = ../evo/evo/ebin
COUCHDB_EBIN = ../../lib/erlang_couchdb/ebin
ID3_EBIN = ../erl_id3v2/ebin

SOURCES  = $(wildcard $(SRC_DIR)/*.erl)
INCLUDES = $(wildcard $(INCLUDE_DIR)/*.hrl)
TARGETS  = $(patsubst $(SRC_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))

QLC_INCLUDE = /usr/local/lib/erlang/lib/stdlib-1.16/include
EVO_INCLUDE = ../evo/evo/src

ERLC_INCLUDES = -I $(INCLUDE_DIR) -I $(QLC_INCLUDE) -I $(EVO_INCLUDE)
ERL_EBINS = -pa $(EBIN_DIR) -pa $(EVO_EBIN) -pa $(ID3_EBIN) \
            -pa $(MOCHIWEB_EBIN) -pa $(COUCHDB_EBIN)

ERLC = erlc
ERLC_OPTS = $(ERLC_INCLUDES) -o $(EBIN_DIR) -Wall -v +debug_info

ERL_CMD=erl \
	-boot start_sasl \
	-config $(PKG_NAME) \
	+W w \
	$(ERL_EBINS)

all: $(TARGETS)

run_prereqs: all

run: run_prereqs
	$(ERL_CMD) -s ${APP_NAME}_app go

setup: run_prereqs
	$(ERL_CMD) -s rubato_app setup -s init stop

test: run_prereqs
	$(ERL_CMD) -s ${APP_NAME}_app go -s rubato_lib test -s init stop

stop:
	erl_call -a '$(APP_NAME)_app stop_and_halt []' -sname $(PKG_NAME)

clean: cleanlog
	rm -f $(TARGETS)
	rm -f $(EBIN_DIR)/*.beam

cleanlog:
	rm -f auth.log report.log sasl_err.log
	rm -f *.access

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_OPTS) $<
