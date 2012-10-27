#
# Tiny C Compiler Makefile
#

TOP ?= .
include $(TOP)/config.mak

CFLAGS+=-g -Wall
CFLAGS += -Wno-pointer-sign -Wno-sign-compare -D_FORTIFY_SOURCE=0 -fno-strict-aliasing
OPTLEVEL = -O0

# XXX remove
LIBS+=-ldl

NATIVE_TARGET=-DTCC_TARGET_X86_64
LIBTCC1=libtcc1.a

ifeq ($(TOP),.)

PROGS=tcc$(EXESUF)

CORE_FILES = tcc.c libtcc.c tccpp.c tccgen.c tccelf.c tccasm.c \
    tcc.h config.h libtcc.h tcctok.h
X86_64_FILES = $(CORE_FILES) x86_64-gen.c

NATIVE_FILES=$(X86_64_FILES)

all: $(PROGS) $(LIBTCC1) $(BCHECK_O) libtcc.a

# Host Tiny C Compiler
tcc$(EXESUF): $(NATIVE_FILES)
	$(CC) -o $@ $< $(NATIVE_TARGET) $(CFLAGS) $(LIBS)

# libtcc generation and test
libtcc.o: $(NATIVE_FILES)
	$(CC) -o $@ -c libtcc.c $(NATIVE_TARGET) $(CFLAGS)

libtcc.a: libtcc.o
	$(AR) rcs $@ $^

libtcc_test$(EXESUF): tests/libtcc_test.c libtcc.a
	$(CC) -o $@ $^ -I. $(CFLAGS) $(LIBS)

libtest: libtcc_test$(EXESUF) $(LIBTCC1)
	./libtcc_test$(EXESUF) lib_path=.

# TinyCC runtime libraries
LIBTCC1_OBJS=libtcc1.o
LIBTCC1_CC=$(CC)
VPATH+=lib

%.o: %.c
	$(LIBTCC1_CC) -o $@ -c $< $(OPTLEVEL) -Wall

%.o: %.S
	$(LIBTCC1_CC) -o $@ -c $<

libtcc1.a: $(LIBTCC1_OBJS)
	$(AR) rcs $@ $^

bcheck.o: bcheck.c
	$(CC) -o $@ -c $< $(OPTLEVEL) -Wall

# clean
clean: local_clean
local_clean:
	rm -vf $(PROGS) tcc_p$(EXESUF) tcc.pod *~ *.o *.a *.out libtcc_test$(EXESUF)

distclean: clean
	rm -vf config.h config.mak config.texi tcc.1 tcc-doc.html

endif # ifeq ($(TOP),.)
