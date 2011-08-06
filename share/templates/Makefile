# $Id: Makefile,v 1.1 2000/08/14 02:33:02 reece Exp $
# Requires GNU make (www.gnu.org)

.SUFFIXES :
.PRECIOUS :
.PHONY : FORCE

OPT:=
DEBUG:=
ARCH:=$(shell uname -sm | tr ' ' -)
prefix:=${HOME}
exec_prefix:=${HOME}/$ARCH
SRCDIRS:=
INCLDIRS:=.:${SRCDIRS}:${prefix}/include

RM:=/bin/rm -f
AR:=/bin/ar -crusv
CC:=g++
cc:=gcc
FC:=f77
LD:=ld

TARGETS:=
OBJECTS:=
RM1:=$(sort )
RM2:=$(sort ${OBJECTS})
RM3:=$(sort ${TARGETS})

INCLDIRECTIVES:=-I$(subst :, -I,${INCLDIRS})
cFLAGS:=${DEBUG} ${OPT} ${INCLDIRECTIVES}
CFLAGS:=${DEBUG} ${OPT} ${INCLDIRECTIVES}
FFLAGS:=${DEBUG} ${OPT} ${INCLDIRECTIVES}
LDFLAGS:=${DEBUG} ${OPT}

cFLAGS:=$(strip ${cFLAGS})
CFLAGS:=$(strip ${CFLAGS})
FFLAGS:=$(strip ${FFLAGS})
LDFLAGS:=$(strip ${LDFLAGS})
&=$(filter-out .%FLAGS,$^)

# vpath %.h
# vpath %.i
# vpath %.a

default all : ${TARGETS}


%.a : ${OBJECTS}
	${AR} $@ $^

% : %.o .LDFLAGS
	${LD} -o $@ ${LDFLAGS} $&
%.o : %.c .cFLAGS
	${cc} -o $@ -c ${cFLAGS} $<
%.o : %.C .CFLAGS
	${CC} -o $@ -c ${CFLAGS} $<
%.o : %.f .FFLAGS
	${FC} -o $@ -c ${FFLAGS} $<


# these files store the current values of the respective variables
# if the flags change, the files are updated and trigger recompilation
# Use the $& variable instead of $^ to strip out any *FLAGS dependencies
.CFLAGS .FFLAGS .LDFLAGS : .% : FORCE
	@echo $*:=${$*} >$@.2
	@diff $@ $@.2 >/dev/null 2>&1; if [ "$$?" != "0" ]; then \
	  mv -f $@.2 $@; \
	  echo $* are new or have changed.;\
	else \
	  rm -f $@.2; \
	fi


.PHONY : clean cleaner cleanest realclean distclean
clean : 
	${RM} ${RM1} 
	${RM} *.bak *~
cleaner realclean : clean
	${RM} ${RM2} 
cleanest distclean : cleaner
	${RM} ${RM3}
	${RM} .*FLAGS
