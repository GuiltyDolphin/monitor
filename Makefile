.PHONY: build install test

build : install
	@cask build

install :
	@cask install

test :
	@cask build \
	&& cask emacs -batch -q -l tests/monitor-tests.el -f ert-run-tests-batch-and-exit
