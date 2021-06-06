.PHONY: test-guile test-gauche test-kawa test-chibi test-chicken

test-guile:
	guile -L . --r7rs {{cookiecutter.name}}-test.scm

test-gauche:
	gosh -I . {{cookiecutter.name}}-test.scm

test-kawa:
	cp {{cookiecutter.name}}.sld {{cookiecutter.name}}.scm
	kawa {{cookiecutter.name}}-test.scm
	rm {{cookiecutter.name}}.scm

test-chibi:
	chibi-scheme {{cookiecutter.name}}-test.scm

test-chicken:
	csc -R r7rs -X r7rs -sJ -o {{cookiecutter.name}}.so {{cookiecutter.name}}.sld
	csi -I . -R r7rs -s {{cookiecutter.name}}-test.scm
	rm {{cookiecutter.name}}.so
	rm {{cookiecutter.name}}.import.scm
