REBAR=./rebar

get_deps:
	@$(REBAR) get-deps

compile: get_deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean

eunit: clean compile
	mv rebar.config rebar.prod.config
	mv rebar.test.config rebar.config
	@$(REBAR) get-deps compile eunit skip_deps=true
	mv rebar.config rebar.test.config
	mv rebar.prod.config rebar.config

ct: clean get_deps compile
#	mv rebar.config rebar.prod.config
#	mv rebar.test.config rebar.config
	@$(REBAR) ct skip_deps=true
#	mv rebar.config rebar.test.config
#	mv rebar.prod.config rebar.config