
.PHONY: test test-interactive

test:
	@echo "Testing..."
	emacs -Q -batch -l lit-parse.el \
					-l lit.el \
					-l test/lit-parse-test.el \
					-l test/lit-test.el\
					-f ert-run-tests-batch-and-exit
