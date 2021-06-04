.PHONY: test-guile test-gauche test-kawa test-chibi test-chicken

test-guile:
	guile -L . --r7rs define-optional-test.scm

test-gauche:
	gosh -I . define-optional-test.scm

test-kawa:
	cp define-optional.sld define-optional.scm
	kawa define-optional-test.scm
	rm define-optional.scm

test-chibi:
	chibi-scheme -I . define-optional-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o define-optional.so define-optional.sld
	csi -I . -R r7rs -s define-optional-test.scm
	rm define-optional.so
	rm define-optional.import.scm
