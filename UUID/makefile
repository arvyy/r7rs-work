.PHONY: test-guile test-gauche test-kawa test-chibi test-chicken

test-guile:
	guile -L . --r7rs uuid-test.scm

test-gauche:
	gosh -I . uuid-test.scm
	#gosh -I . sha-1-test.scm

test-kawa:
	cp uuid.sld uuid.scm
	kawa uuid-test.scm
	rm uuid.scm

test-chibi:
	chibi-scheme uuid-test.scm
	#chibi-scheme sha-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o uuid.so uuid.sld
	csi -I . -R r7rs -s uuid-test.scm
	rm uuid.so
	rm uuid.import.scm
