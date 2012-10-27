#
# Tiny C Compiler Makefile
#

CFLAGS += -g -Wall
CFLAGS += -Wno-pointer-sign -Wno-sign-compare -fno-strict-aliasing
OPTLEVEL = -O0

PROGS=tcc$(EXESUF)

CORE_FILES = tcc.c libtcc.c tccpp.c tccgen.c \
    tcc.h libtcc.h tcctok.h

all: $(PROGS) $(LIBTCC1) $(BCHECK_O)

# Host Tiny C Compiler
tcc$(EXESUF): $(CORE_FILES)
	$(CC) -o $@ $< $(CFLAGS) $(LIBS)

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

