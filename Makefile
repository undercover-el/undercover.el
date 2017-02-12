# The MIT License (MIT)

# Copyright (c) 2014-2017 Sviridov Alexander

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE

APP = undercover

SHELL = /bin/bash

EMACS ?= emacs
EMACSFLAGS = -L .
CASK = cask

VERSION=$(shell \
        grep ";; Version" undercover.el \
        |awk -F':' '{print $$2}' \
	|sed -e "s/[^0-9.]//g")

ELS = $(shell find . -name "*.el")
OBJECTS = $(ELS:.el=.elc)

NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m

all: help

help:
	@echo -e "$(OK_COLOR)==== $(APP) [$(VERSION)]====$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- init$(NO_COLOR)             : initialize development environment"
	@echo -e "$(WARN_COLOR)- build$(NO_COLOR)            : build project"
	@echo -e "$(WARN_COLOR)- test$(NO_COLOR)             : launch unit tests"
	@echo -e "$(WARN_COLOR)- test-tag tag=xxx$(NO_COLOR) : launch unit tests"
	@echo -e "$(WARN_COLOR)- clean$(NO_COLOR)            : cleanup"

init:
	@echo -e "$(OK_COLOR)[$(APP)] Initialize environment$(NO_COLOR)"
	@$(CASK) --dev install

elpa:
	@echo -e "$(OK_COLOR)[$(APP)] Build$(NO_COLOR)"
	@$(CASK) install
	@$(CASK) update
	@touch $@

.PHONY: build
build : elpa $(OBJECTS)

test: build
	@echo -e "$(OK_COLOR)[$(APP)] Unit tests$(NO_COLOR)"
	@$(CASK) exec ert-runner

test-tag: build
	@echo -e "$(OK_COLOR)[$(APP)] Unit tests$(NO_COLOR)"
	@$(CASK) exec ert-runner --verbose --debug -t $(tag)

.PHONY: clean
clean :
	@echo -e "$(OK_COLOR)[$(APP)] Cleanup$(NO_COLOR)"
	@rm -fr $(OBJECTS) elpa

reset : clean
	@rm -rf .cask

%.elc : %.el
	@$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<
