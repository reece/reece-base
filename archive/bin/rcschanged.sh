#!/bin/sh

for f in $*
do
	rcsdiff $f >/dev/null 2>/dev/null
	[ $? = 1 ] && echo $f
done
	