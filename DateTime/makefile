.PHONY: test-guile test-gauche test-kawa test-chibi test-chicken

test-guile:
	guile -L . --r7rs date-time-test.scm

test-gauche:
	gosh -I . date-time-test.scm

test-kawa:
	cp date-time.sld date-time.scm
	kawa date-time-test.scm
	rm date-time.scm

test-chibi:
	chibi-scheme date-time-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o date-time.so date-time.sld
	csi -I . -R r7rs -s date-time-test.scm
	rm date-time.so
	rm date-time.import.scm
