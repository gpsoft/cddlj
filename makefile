IMAGENAME ?= mysql:5.7.23
CONTAINERNAME ?= mysql

all:
	@echo Usage:
	@echo make mysql
	@echo make attach
	@echo make uberjar

.PHONY: mysql attach uberjar

mysql:
	docker run --rm -it \
		--env MYSQL_ROOT_PASSWORD=mysql \
		--name $(CONTAINERNAME) \
		-p 3306:3306 \
		$(IMAGENAME)

# Attach to the running container.
attach:
	docker exec -it \
		$(CONTAINERNAME) bash

uberjar: cddlj.jar

cddlj.jar: $(wildcard src/**/*.clj* resources/*.*)
	lein uberjar
	cp target/uberjar/cddlj-0.1-standalone.jar cddlj.jar

.SILENT:
%:
	@:


