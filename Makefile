TARGETS ?= tcc$(EXE_SUFFIX)
CC_OPT = -O2
# add -g to CC_DEBUG on command-line
CC_DEBUG =

all: $(TARGETS)

$(TARGETS): CFLAGS += $(CC_DEBUG) -Wall
$(TARGETS): CFLAGS += -Wno-pointer-sign -Wno-sign-compare -fno-strict-aliasing
$(TARGETS): CFLAGS += $(CC_OPT)

tcc.o: libtcc.c tccpp.c tccgen.c tcc.h libtcc.h tcctok.h
tcc$(EXE_SUFFIX): tcc.o
	$(LINK.c) -o $@ $< $(LDLIBS)

CLEANFILES += $(TARGETS) *.o *.dSYM
clean::
	rm -rf $(CLEANFILES)

