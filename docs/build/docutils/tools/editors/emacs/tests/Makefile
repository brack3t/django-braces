#!/usr/bin/env make

EMACS = /usr/bin/emacs
ERT = ~/lib/emacs/ert

EMACS_ERT_PFX = $(EMACS) --batch --quick --directory=$(ERT) --load=ert.el
EMACS_ERT_SFX = --funcall=ert-run-tests-batch-and-exit

ERT_TESTS := $(filter-out ert-%,$(wildcard *.el))

all:

tests:
	$(EMACS_ERT_PFX) $(addprefix -l ,$(ERT_TESTS)) $(EMACS_ERT_SFX)

clean:
