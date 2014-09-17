TARGETS ?= tcc$(EXE_SUFFIX)

all: $(TARGETS)

$(TARGETS): CFLAGS += -g -Wall
$(TARGETS): CFLAGS += -Wno-pointer-sign -Wno-sign-compare -fno-strict-aliasing
# clang doesn't know we are using old-style variable-sized structs
$(TARGETS): CFLAGS += -O2

tcc.o: libtcc.c tccpp.c tccgen.c tcc.h libtcc.h tcctok.h
tcc$(EXE_SUFFIX): tcc.o
	$(LINK.c) -o $@ $< $(LDLIBS)

CLEANFILES += $(TARGETS) *.o *.dSYM
clean::
	rm -rf $(CLEANFILES)

