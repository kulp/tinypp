#
# Tiny C Compiler Makefile
#

TOP ?= .
include $(TOP)/config.mak

CFLAGS+=-g -Wall
CFLAGS += -Wno-pointer-sign -Wno-sign-compare -D_FORTIFY_SOURCE=0 -fno-strict-aliasing
OPTLEVEL = -O0

NATIVE_TARGET=-DTCC_TARGET_X86_64

ifeq ($(TOP),.)

PROGS=tcc$(EXESUF)

CORE_FILES = tcc.c libtcc.c tccpp.c tccgen.c \
    tcc.h config.h libtcc.h tcctok.h
X86_64_FILES = $(CORE_FILES) x86_64-defs.c

NATIVE_FILES=$(X86_64_FILES)

all: $(PROGS) $(LIBTCC1) $(BCHECK_O)

# Host Tiny C Compiler
tcc$(EXESUF): $(NATIVE_FILES)
	$(CC) -o $@ $< $(NATIVE_TARGET) $(CFLAGS) $(LIBS)

%.o: %.c
	$(LIBTCC1_CC) -o $@ -c $< $(OPTLEVEL) -Wall

%.o: %.S
	$(LIBTCC1_CC) -o $@ -c $<

libtcc1.a: $(LIBTCC1_OBJS)
	$(AR) rcs $@ $^

# clean
clean: local_clean
local_clean:
	rm -vf $(PROGS) *~ *.o *.a *.out

distclean: clean
	rm -vf config.h config.mak config.texi

endif # ifeq ($(TOP),.)
