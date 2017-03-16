.PHONY: default
default: run;

build:
	docker build . -t pewpew

run:
	docker run -i -t -p 4321:4321 pewpew
