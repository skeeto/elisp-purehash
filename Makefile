EMACS   ?= emacs
BATCH   := $(EMACS) -batch -Q -L .
COMPILE := $(BATCH) -f batch-byte-compile
TEST    := $(BATCH) -L tests -l purehash-tests.elc -f ert-run-tests-batch

EL = purehash.el purehash-tests.el

ELC = $(EL:.el=.elc)

.PHONY : all compile test clean

all : test

compile: $(ELC)

test: compile
	$(TEST) -f purehash-benchmark

clean:
	$(RM) *.elc

%.elc: %.el
	@$(COMPILE) $<
