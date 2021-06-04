.PHONY: test-guile test-gauche test-kawa test-chibi

test-guile:
	guile -L . --r7rs --fresh-auto-compile define-optional-test.scm

test-gauche:
	gosh -I . define-optional-test.scm

test-kawa:
	cp define-optional.sld define-optional.scm
	kawa define-optional-test.scm
	rm define-optional.scm

test-chibi:
	chibi-scheme -I . define-optional-test.scm

# Chicken fails with
# Syntax error: Cannot combine dotted tail and ellipsis
# 	(var ... . rest)
