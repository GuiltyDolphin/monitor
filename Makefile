.PHONY: build clean doc install test

clean :
	@cask clean-elc

build : clean install
	@cask build

doc :
	@cd doc && make all

install :
	@cask install

test : clean
	@cask build \
	&& cask emacs -batch -q -l tests/monitor-tests.el -f ert-run-tests-batch-and-exit
