flp19-log: flp-turing.pl
	swipl -q -g start -o flp19-log -c flp-turing.pl

TESTS=$(wildcard test/*-input.txt)

.PHONY: test
test: flp19-log $(TESTS)
	test/run.sh $(TESTS)
