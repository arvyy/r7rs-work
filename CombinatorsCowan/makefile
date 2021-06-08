.PHONY: test-guile test-gauche test-kawa test-chibi test-chicken

test-guile:
	guile -L . --r7rs combinators-test.scm

test-gauche:
	gosh -I . combinators-test.scm

test-kawa:
	cp combinators.sld combinators.scm
	kawa combinators-test.scm
	rm combinators.scm

test-chibi:
	chibi-scheme combinators-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o combinators.so combinators.sld
	csi -I . -R r7rs -s combinators-test.scm
	rm combinators.so
	rm combinators.import.scm
