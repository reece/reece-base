.PHONY: FORCE
.SUFFIXES:
.DELETE_ON_ERROR:




.PHONY: clean cleaner cleanest
clean:
	find . -name \*~ -print0 | xargs -0r /bin/rm
cleaner: clean
	find . \( -name \*bak -o -name \*.orig \) -print0 | xargs -0r /bin/rm
cleanest: cleaner
