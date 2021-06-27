.PHONY: test-gauche test-chibi

# bug with make-vector-comparator?
test-gauche:
	gosh -I . uuid-test.scm
	gosh -I . sha-1-test.scm

test-chibi:
	chibi-scheme uuid-test.scm
	chibi-scheme sha-1-test.scm
