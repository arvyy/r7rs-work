test:
	touch conditions.log
	docker run --mount type=bind,source=$$PWD/conditions.log,target=/conditions.log --rm -it $$(docker build -q .)
