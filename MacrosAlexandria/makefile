.PHONY: test-guile test-gauche test-kawa test-chibi

# Fails on Guile v3.0.5 (bug?)
test-guile:
	guile -L . --r7rs --fresh-auto-compile misc-let-test.scm

test-gauche:
	gosh -I . misc-let-test.scm

test-kawa:
	cp misc-let.sld misc-let.scm
	kawa misc-let-test.scm
	rm misc-let.scm

test-chibi:
	chibi-scheme misc-let-test.scm

# Chicken fails with
# Syntax error: Cannot combine dotted tail and ellipsis
# 	(var ... . rest)
