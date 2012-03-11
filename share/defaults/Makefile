.PHONY: FORCE
.SUFFIXES:
.DELETE_ON_ERROR:

SHELL:=/bin/bash
SELF:=$(firstword $(MAKEFILE_LIST))
ROOT_DIR:=$(shell readlink -f $$(dirname $$(readlink -f "${SELF}"))/../..)
PATH:=${ROOT_DIR}/bin:${PATH}

default: 
	@echo "Ain't no stinking $@ target, homey!" 1>&2; exit 1

env:
	@echo SHELL=${SHELL}
	@echo SELF=${SELF}
	@echo ROOT_DIR=${ROOT_DIR}
	@echo CONDITIONS=${CONDITIONS}



.PHONY: clean cleaner cleanest
clean:
	find . -name \*~ -print0 | xargs -0r /bin/rm
cleaner: clean
	find . \( -name \*bak -o -name \*.orig \) -print0 | xargs -0r /bin/rm
cleanest: cleaner
