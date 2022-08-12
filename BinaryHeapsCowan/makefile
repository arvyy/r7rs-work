.PHONY: test-gauche test-chibi test-chicken

test-gauche:
	gosh -I . binary-heap-test.scm

test-chibi:
	chibi-scheme binary-heap-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o binary-heap.so binary-heap.sld
	csi -I . -R r7rs -s binary-heap-test.scm
	rm binary-heap.so
	rm binary-heap.import.scm
