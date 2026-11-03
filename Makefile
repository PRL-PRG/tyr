.PHONY: build test coverage clean

SHELL := /bin/bash

# Rsem and NativeSem both link against tree-sitter; NativeSem's setup script
# derives the paths from an r-parser checkout. Its own default (next to
# NativeSem) is not the right one here, since NativeSem is a submodule, so look
# for r-parser inside TypR first and next to it otherwise. r-parser is not a
# submodule on purpose: what the build needs from it (core/tree-sitter) is
# produced by its own `make setup`, not by checking it out.
# Override by setting R_PARSER_PATH in the environment.
export R_PARSER_PATH ?= $(firstword \
  $(wildcard $(CURDIR)/r-parser) $(CURDIR)/../r-parser)

define WITH_ENV
	source ./nativesem/setup-env.sh && eval "$$(opam env)" && $(1)
endef

build:
	@$(call WITH_ENV,dune build)

test:
	@$(call WITH_ENV,dune runtest)

# tyr's own tests, instrumented: raw data in _coverage/, the HTML report in
# _coverage/html/index.html.
coverage:
	@rm -rf _coverage && mkdir -p _coverage
	@$(call WITH_ENV,BISECT_FILE=$(CURDIR)/_coverage/bisect dune runtest test --instrument-with bisect_ppx --force)
	@$(call WITH_ENV,bisect-ppx-report html --coverage-path _coverage -o _coverage/html)
	@$(call WITH_ENV,bisect-ppx-report summary --coverage-path _coverage)

clean:
	@dune clean
	@rm -rf _coverage
