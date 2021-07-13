.PHONY: test-gauche test-kawa test-chibi test-chicken

test-gauche:
	gosh -I . pathnames-test.scm

test-kawa:
	cp pathnames.sld pathnames.scm
	kawa pathnames-test.scm
	rm pathnames.scm

test-chibi:
	chibi-scheme pathnames-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o pathnames.so pathnames.sld
	csi -I . -R r7rs -s pathnames-test.scm
	rm pathnames.so
	rm pathnames.import.scm
