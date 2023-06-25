
.PHONY: test test-interactive

test:
	@echo "Testing..."
	emacs -Q -batch --eval "(progn\
	(add-to-list 'load-path \".\")\
	(add-to-list 'load-path \"..\")\
	(add-to-list 'load-path \"./test\")\
	(require 'lit-test)\
	(require 'lit-parse-test)\
	(ert-run-tests-batch-and-exit))"

test-interactive:
	emacs -Q --debug-init --eval "(progn\
	(add-to-list 'load-path \".\")\
	(add-to-list 'load-path \"..\")\
	(add-to-list 'load-path \"./test\")\
	(require 'lit-test)\
	(require 'lit-parse-test)\
	(ert t))"
