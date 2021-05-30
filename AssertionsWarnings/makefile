.PHONY: test-guile test-gauche test-kawa test-chibi test-chicken

test-guile:
	guile -L . --r7rs assertions-test.scm

test-gauche:
	gosh -I . assertions-test.scm

test-kawa:
	cp assertions.sld assertions.scm
	kawa assertions-test.scm
	rm assertions.scm

test-chibi:
	chibi-scheme assertions-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o assertions.so assertions.sld
	csi -I . -R r7rs -s assertions-test.scm
	rm assertions.so
	rm assertions.import.scm
