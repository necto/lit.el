
.PHONY: test test-interactive

test:
	@echo "Testing..."
	emacs -Q -batch -l lit-parse.el \
					-l lit.el \
					-l test/lit-parse-test.el \
					-l test/lit-test.el\
					-f ert-run-tests-batch-and-exit

coverage-interactive:
	emacs   -l lit-parse.el \
			-l lit.el \
			-l test/lit-parse-test.el \
			-l test/lit-test.el \
			--eval "(progn \
						(testcover-start \"lit.el\")\
						(testcover-start \"lit-parse.el\")\
						(ert-run-tests-batch)\
						(testcover-mark-all \"lit-parse.el\")\
						(testcover-mark-all \"lit.el\"))"
