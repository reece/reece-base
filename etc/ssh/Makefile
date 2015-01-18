.SUFFIXES:
.DELETE_ON_ERROR:

authorized_keys: $(wildcard keys/*.pub)
	sort -u $@ $^ >$@.tmp
	mv $@.tmp $@

