ERLFLAGS= -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=./rebar

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

.PHONY: all compile doc clean test shell distclean \
  update-deps rebuild run

all: deps compile test

# =============================================================================
# Rules to build the system
# =============================================================================

deps:
	$(REBAR) get-deps
	$(REBAR) compile

compile:
	$(REBAR) compile

doc:
	$(REBAR) skip_deps=true doc

eunit: compile
	$(REBAR) skip_deps=true eunit

test: eunit

shell: deps compile
	@$(ERL) $(ERLFLAGS)

clean:
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/log
	- rm -rf $(CURDIR)/ebin
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rvf $(CURDIR)/deps

rebuild: distclean deps compile test

run: compile
	@PEWPEW_ENV=${MODE} $(ERL) $(ERLFLAGS) -config app -s pewpew
