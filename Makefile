CFLAGS += -g -Wall
CFLAGS += -Wno-pointer-sign -Wno-sign-compare -fno-strict-aliasing
# clang doesn't know we are using old-style variable-sized structs
CFLAGS += -Wno-array-bounds
CFLAGS += -O2

all: tcc

tcc: tcc.c libtcc.c tccpp.c tccgen.c tcc.h libtcc.h tcctok.h
	$(CC) -o $@ $< $(CFLAGS) $(LIBS)

clean:
	rm -rf tcc *.o *.dSYM

