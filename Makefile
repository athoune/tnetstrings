REBAR = ./rebar

all: app

app: get-deps
	@$(REBAR) compile

get-deps:
	@$(REBAR) get-deps

tests: clean app eunit

eunit:
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

dist-clean: clean
