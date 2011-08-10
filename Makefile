.SUFFIXES:
.PHONY: FORCE
.DELETE_ON_ERROR:

default:
	@echo no $@ target 1>&2; exit 1

DT:=$(shell /bin/date +'%Y%m%dT%H%M%S')
SVNVERSION:=$(shell svnversion | tr : -)
PFX:=reece-base-${SVNVERSION}

tar: ../${PFX}.tar.bz2

../${PFX}.tar.bz2: FORCE
	cd ..; tar -cj --exclude-from=reece-base/.excludes -f ${@F} reece-base

pull:
	hg pull -u

push:
	hg push ssh://hg@bitbucket.org/reece/reece-base
	hg push git+ssh://git@github.com/reecehart/reece-base.git

.PHONY: clean cleaner cleanest
clean:
	find . \( -name \*~ -o -name \*.bak -o -name \#\* \) -print0 | xargs -0r /bin/rm -v
cleaner: clean
cleanest: cleaner
