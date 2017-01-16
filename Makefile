REBAR := `pwd`/rebar3

all: test doc release

compile:
	@$(REBAR) compile

doc:
	@$(REBAR) edoc

test:
	@$(REBAR) do xref, dialyzer, eunit, ct, cover

release:
	@$(REBAR) release

tar:
	@$(REBAR) as prod tar

clean:
	@$(REBAR) clean

shell:
	@$(REBAR) shell

run:
	@$(REBAR) run

.PHONY: release test all compile clean
