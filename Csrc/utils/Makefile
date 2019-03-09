#
# $Id: Makefile 396 2019-01-02 22:53:10Z viari $
#

include Makefile.common

# -------------------------------------
# library sources
#

LIB_SRC = libsys.c      \
          libmemory.c   \
          libbinio.c    \
          libstring.c   \
          libcrc32.c    \
          libfasta.c    \
          libbasta.c    \
          libbgz.c

LIB_OBJ = $(LIB_SRC:.c=.o)

LIB = liblx.a

PRG = fasta2basta bam2baf

PRTDIR = ../../ports/$(PRTNAME)

ZLBDIR = ../zlib
DFLDIR = ../libdeflate
HTSDIR = ../htslib
SAMDIR = ../samtools

ZLBLIB = $(ZLBDIR)/libz.a
DFLLIB = $(DFLDIR)/libdeflate.a
HTSLIB = $(HTSDIR)/libhts.a
SAMLIB = $(SAMDIR)/libbam.a

CPPFLAGS += -I$(ZLBDIR) -I$(HTSDIR) -I$(SAMDIR)

LDFLAGS = -L. -L$(ZLBDIR) -L$(DFLDIR) -L$(HTSDIR) -L$(SAMDIR)

LDLIBS = $(LIB) $(SAMLIB) $(HTSLIB) $(DFLLIB) $(ZLBLIB) -lm -lpthread

ifdef WITH_BZ2
LDLIBS += -lbz2
endif

ifdef WITH_LZMA
LDLIBS += -lzma
endif

# -------------------------------------
# rules
#

.PHONY: all notify lib notify prg instdir install test clean distclean

all: notify lib prg

notify:
	@echo "----------------------------------------"
	@echo " make utils C sources"
	@echo "----------------------------------------"

lib: $(LIB)

$(LIB): $(LIB_OBJ)
	$(AR) $(ARFLAGS) $(LIB) $(LIB_OBJ)
	$(RANLIB) $(LIB)

prg: $(PRG)

instdir:
	-@(test -d $(PRTDIR))     || ($(MKDIR) $(PRTDIR))
	-@(test -d $(PRTDIR)/bin) || ($(MKDIR) $(PRTDIR)/bin)

install: instdir
	-$(CP) $(PRG) $(PRTDIR)/bin

test:
	cd test && make test

clean:
	-$(RM) -r -f *.o $(LIB) $(PRG) *.dSYM
	-cd test && make clean

distclean: clean
	# remove binaries ?
