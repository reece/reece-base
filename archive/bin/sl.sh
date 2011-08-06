#!/bin/sh
# sl - sever link
# replace a symbolic or hard linked file with a new file of identical contents
# $Id: sl.sh,v 0.3 1998/11/16 14:38:14 reece Exp $
# Reece Kimball Hart <reece@dasher.wustl.edu>

#PATH:=/bin
while [ $# -gt 0 ]; do
	p="$1"
	shift

	if [ ! -L "$p" ]; then
		echo "$0: $p: not a symbolic link"
		continue
	fi

	d=`dirname "$p"`
	f=`basename "$p"`
	t="$d/.$f"

	if [ -f "$t" ]; then
		echo "$0: .$t: exists" 1>&2
		continue
	fi

	cmd="/bin/cp \"$p\" \"$t\""
	if eval $cmd; then
		:
	else
		echo "$0: $cmd: failed" 1>&2
		continue
	fi

	cmd="/bin/mv -f \"$t\" \"$p\""
	if eval $cmd; then
		:
	else
		echo "$0: $cmd: failed" 1>&2
		/bin/rm -f "$t"
	fi
done
