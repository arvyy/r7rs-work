.PHONY: test-guile test-gauche test-chibi test-chicken

test-guile:
	guile -L . --r7rs iteration-cl-test.scm

test-gauche:
	gosh -I . iteration-cl-test.scm

test-chibi:
	chibi-scheme iteration-cl-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o iteration-cl.so iteration-cl.sld
	csi -I . -R r7rs -s iteration-cl-test.scm
	rm iteration-cl.so
	rm iteration-cl.import.scm

# Kawa fails to run due to limitation for reentrant continuations
