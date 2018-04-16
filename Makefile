REBAR3 ?= rebar3

.PHONY: all debug test fmt

all:
	$(REBAR3) compile
	$(REBAR3) as test compile
	$(REBAR3) xref
	$(REBAR3) as test xref

debug:
	$(REBAR3) shell

test:
	$(REBAR3) cover --reset
	$(REBAR3) proper
	$(REBAR3) cover

FMT = _build/erlang-formatter-master/fmt.sh
$(FMT):
	mkdir -p _build/
	curl -f#SL 'https://codeload.github.com/fenollp/erlang-formatter/tar.gz/master' | tar xvz -C _build/
fmt: TO_FMT ?= .
fmt: $(FMT)
	$(if $(TO_FMT), $(FMT) $(TO_FMT))
