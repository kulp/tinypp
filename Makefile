TARGETS ?= tcc

all: $(TARGETS)

$(TARGETS): CFLAGS += -g -Wall
$(TARGETS): CFLAGS += -Wno-pointer-sign -Wno-sign-compare -fno-strict-aliasing
# clang doesn't know we are using old-style variable-sized structs
$(TARGETS): CFLAGS += -Wno-array-bounds
$(TARGETS): CFLAGS += -O2

tcc.o: libtcc.c tccpp.c tccgen.c tcc.h libtcc.h tcctok.h
tcc$(EXE_SUFFIX): tcc.o
	$(LINK.c) -o $@ $< $(LDLIBS)

CLEANFILES += tcc *.o *.dSYM
clean::
	rm -rf $(CLEANFILES)

