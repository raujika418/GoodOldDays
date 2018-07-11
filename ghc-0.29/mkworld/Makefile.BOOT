#
#   WARNING    WARNING    WARNING    WARNING    WARNING    WARNING    WARNING
#
# This is NOT an automatically generated Makefile!  It is hand-crafted as a 
# bootstrap, may need editing for your system.  The BOOTSTRAPCFLAGS variable
# may be given at the top of the build tree for systems that do not define
# any machine-specific preprocessor symbols.
#

BOOTSTRAPCFLAGS = 
PROJECTLABEL = none
SETUPLABEL = std
CDEBUGFLAGS = 
CFLAGS = $(BOOTSTRAPCFLAGS) $(CDEBUGFLAGS)
SHELL = /bin/sh
RM = rm -f
MV = mv -f
MAKE = make
CLEAN_CMD = $(RM) *.CKP *.ln *.BAK *.bak *.o core errs ,* *~ *.a tags TAGS make.log
NPROC = 1
CC = gcc

TOP = ..
CURRENT_DIR = ./mkworld

JMAKESRC = $(TOP)/mkworld
JMAKE    = $(JMAKESRC)/jmake

JMAKE_CMD = $(JMAKE) -I$(NEWTOP)$(JMAKESRC) $(BOOTSTRAPCFLAGS) -DTopDir=$(TOP) -DCurDir=$(CURRENT_DIR) $(BOOT_DEFINES)

all: jmake Makefile

jmake:: ccjmake jmake.c
	@echo "making jmake with BOOTSTRAPCFLAGS=$(BOOTSTRAPCFLAGS)"
	$(CC) -o jmake $(CFLAGS) jmake.c `./ccjmake`

ccjmake: ccjmake.c
	$(CC) -o ccjmake $(CFLAGS) ccjmake.c

relink:
	$(RM) jmake
	$(MAKE) $(MFLAGS) jmake

clean:
	$(RM) ccjmake jmake
	$(RM) \#*

depend:


# for manual use

Makefile ::
	-@if [ -f Makefile ]; then \
	echo "	$(RM) Makefile.bak; $(MV) Makefile Makefile.bak"; \
	$(RM) Makefile.bak; $(MV) Makefile Makefile.bak; \
	else exit 0; fi
	$(JMAKE_CMD)
	@if cmp -s Makefile Makefile.bak; then $(RM) Makefile.bak ; else exit 0 ; fi
	@chmod 444 Makefile
	@echo ==== The new Makefile is for\: ====
	@$(MAKE) whoami

tags:

install:

install_docs:

Makefiles:
