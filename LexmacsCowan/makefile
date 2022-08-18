.PHONY: test-gauche test-kawa test-chibi test-chicken

test-gauche:
	gosh -I . lexmacs-test.scm

test-kawa:
	cp lexmacs.sld lexmacs.scm
	kawa lexmacs-test.scm
	rm lexmacs.scm

test-chibi:
	chibi-scheme lexmacs-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o lexmacs.so lexmacs.sld
	csi -I . -R r7rs -s lexmacs-test.scm
	rm lexmacs.so
	rm lexmacs.import.scm
