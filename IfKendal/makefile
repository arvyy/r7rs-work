.PHONY: test-guile test-gauche test-kawa test-chibi test-chicken

test-guile:
	guile -L . --r7rs if-star-test.scm

test-gauche:
	gosh -I . if-star-test.scm

test-kawa:
	cp if-star.sld if-star.scm
	kawa if-star-test.scm
	rm if-star.scm

test-chibi:
	chibi-scheme if-star-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o if-star.so if-star.sld
	csi -I . -R r7rs -s if-star-test.scm
	rm if-star.so
	rm if-star.import.scm
