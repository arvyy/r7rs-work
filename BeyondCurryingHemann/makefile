.PHONY: test-guile test-gauche test-kawa test-chibi

test-guile:
	guile -L . --r7rs lambda-star-test.scm

test-gauche:
	gosh -I . lambda-star-test.scm

test-kawa:
	cp lambda-star.sld lambda-star.scm
	kawa lambda-star-test.scm
	rm lambda-star.scm

test-chibi:
	chibi-scheme lambda-star-test.scm

# Chicken fails to run with error
#
# Syntax error: Cannot combine dotted tail and ellipsis
#	(a* ... . rest)
