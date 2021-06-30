.PHONY: test-guile test-gauche test-kawa test-chibi test-chicken

test-guile:
	guile -L . --r7rs restarts-test.scm

test-gauche:
	gosh -I . restarts-test.scm

test-kawa:
	cp restarts.sld restarts.scm
	kawa restarts-test.scm
	rm restarts.scm

test-chibi:
	chibi-scheme restarts-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o restarts.so restarts.sld
	csi -I . -R r7rs -s restarts-test.scm
	rm restarts.so
	rm restarts.import.scm
