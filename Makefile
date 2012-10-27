#
# Tiny C Compiler Makefile
#

TOP ?= .
include $(TOP)/config.mak

CFLAGS+=-g -Wall
CFLAGS_P=$(CFLAGS) -pg -static -DCONFIG_TCC_STATIC
LIBS_P=

ifneq ($(GCC_MAJOR),2)
CFLAGS+=-fno-strict-aliasing
endif

ifeq ($(ARCH),i386)
CFLAGS+=-mpreferred-stack-boundary=2
ifeq ($(GCC_MAJOR),2)
CFLAGS+=-m386 -malign-functions=0
else
CFLAGS+=-march=i386 -falign-functions=0
ifneq ($(GCC_MAJOR),3)
CFLAGS+=-Wno-pointer-sign -Wno-sign-compare -D_FORTIFY_SOURCE=0
endif
endif
endif

ifeq ($(ARCH),x86-64)
CFLAGS+=-Wno-pointer-sign
endif

ifndef CONFIG_WIN32
LIBS=-lm
ifndef CONFIG_NOLDL
LIBS+=-ldl
endif
endif

ifdef CONFIG_WIN32
NATIVE_TARGET=-DTCC_TARGET_PE
LIBTCC1=libtcc1.a
else
ifeq ($(ARCH),i386)
NATIVE_TARGET=-DTCC_TARGET_I386
LIBTCC1=libtcc1.a
BCHECK_O=bcheck.o
else
ifeq ($(ARCH),arm)
NATIVE_TARGET=-DTCC_TARGET_ARM
NATIVE_TARGET+=$(if $(wildcard /lib/ld-linux.so.3),-DTCC_ARM_EABI)
NATIVE_TARGET+=$(if $(shell grep -l "^Features.* \(vfp\|iwmmxt\) " /proc/cpuinfo),-DTCC_ARM_VFP)
else
ifeq ($(ARCH),x86-64)
NATIVE_TARGET=-DTCC_TARGET_X86_64
LIBTCC1=libtcc1.a
endif
endif
endif
endif

ifneq ($(wildcard /lib/ld-uClibc.so.0),)
NATIVE_TARGET+=-DTCC_UCLIBC
BCHECK_O=
endif

ifdef CONFIG_USE_LIBGCC
LIBTCC1=
endif

ifeq ($(TOP),.)

PROGS=tcc$(EXESUF)

CORE_FILES = tcc.c libtcc.c tccpp.c tccgen.c tccelf.c tccasm.c \
    tcc.h config.h libtcc.h tcctok.h
I386_FILES = $(CORE_FILES) i386-gen.c i386-asm.c i386-asm.h
WIN32_FILES = $(CORE_FILES) i386-gen.c i386-asm.c i386-asm.h tccpe.c
X86_64_FILES = $(CORE_FILES) x86_64-gen.c
ARM_FILES = $(CORE_FILES) arm-gen.c
C67_FILES = $(CORE_FILES) c67-gen.c tcccoff.c

ifdef CONFIG_WIN32
PROGS+=tiny_impdef$(EXESUF) tiny_libmaker$(EXESUF)
NATIVE_FILES=$(WIN32_FILES)
else
ifeq ($(ARCH),i386)
NATIVE_FILES=$(I386_FILES)
else
ifeq ($(ARCH),x86-64)
NATIVE_FILES=$(X86_64_FILES)
else
ifeq ($(ARCH),arm)
NATIVE_FILES=$(ARM_FILES)
endif
endif
endif
endif

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
ifdef CONFIG_WIN32
# for windows, we must use TCC because we generate ELF objects
LIBTCC1_OBJS+=crt1.o wincrt1.o dllcrt1.o dllmain.o chkstk.o
LIBTCC1_CC=./tcc.exe -Bwin32 -DTCC_TARGET_PE
VPATH+=win32/lib
endif
ifeq ($(ARCH),i386)
LIBTCC1_OBJS+=alloca86.o alloca86-bt.o
endif

%.o: %.c
	$(LIBTCC1_CC) -o $@ -c $< -O2 -Wall

%.o: %.S
	$(LIBTCC1_CC) -o $@ -c $<

libtcc1.a: $(LIBTCC1_OBJS)
	$(AR) rcs $@ $^

bcheck.o: bcheck.c
	$(CC) -o $@ -c $< -O2 -Wall

# clean
clean: local_clean
local_clean:
	rm -vf $(PROGS) tcc_p$(EXESUF) tcc.pod *~ *.o *.a *.out libtcc_test$(EXESUF)

distclean: clean
	rm -vf config.h config.mak config.texi tcc.1 tcc-doc.html

endif # ifeq ($(TOP),.)
