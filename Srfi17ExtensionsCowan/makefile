.PHONY: test-guile test-gauche test-kawa test-chicken

test-guile:
	guile -L . --r7rs --fresh-auto-compile srfi-17-ext-test.scm

test-gauche:
	gosh -I . srfi-17-ext-test.scm

test-kawa:
	cp srfi-17-ext.sld srfi-17-ext.scm
	kawa srfi-17-ext-test.scm
	rm srfi-17-ext.scm

#chibi doesn't implement srfi 17

test-chicken:
	csc -R r7rs -X r7rs -sJ -o srfi-17-ext.so srfi-17-ext.sld
	csi -I . -R r7rs -s srfi-17-ext-test.scm
	rm srfi-17-ext.so
	rm srfi-17-ext.import.scm
