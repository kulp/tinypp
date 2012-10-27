#
# Tiny C Compiler Makefile
#

CFLAGS += -g -Wall
CFLAGS += -Wno-pointer-sign -Wno-sign-compare -fno-strict-aliasing
# clang doesn't know we are using old-style variable-sized structs
CFLAGS += -Wno-array-bounds
CFLAGS += -O2

PROGS = tcc$(EXESUF)

all: $(PROGS)

# Host Tiny C Compiler
tcc$(EXESUF):  tcc.c libtcc.c tccpp.c tccgen.c tcc.h libtcc.h tcctok.h
	$(CC) -o $@ $< $(CFLAGS) $(LIBS)

# clean
clean:
	rm -f $(PROGS) *.o

