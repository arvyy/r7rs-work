.PHONY: test-guile test-gauche test-kawa test-chibi test-chicken

test-guile:
	guile -L . --r7rs let-settings-test.scm

test-gauche:
	gosh -I . let-settings-test.scm

test-kawa:
	cp let-settings.sld let-settings.scm
	kawa let-settings-test.scm
	rm let-settings.scm

test-chibi:
	chibi-scheme let-settings-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o let-settings.so let-settings.sld
	csi -I . -R r7rs -s let-settings-test.scm
	rm let-settings.so
	rm let-settings.import.scm
