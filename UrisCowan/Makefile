.PHONY: test-gauche test-chibi test-gauche-docker

test-gauche:
	gosh -I . uris-test.scm

test-chibi:
	chibi-scheme uris-test.scm

test-gauche-docker:
	docker run --mount type=bind,source="$(shell pwd)",target=/app --workdir=/app --rm -it schemers/gauche gosh -I . uris-test.scm
