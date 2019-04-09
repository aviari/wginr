#
# $Id: Makefile 474 2019-01-10 23:38:37Z viari $
#

# standard commands
CP      = cp
RM      = rm
MKDIR   = mkdir

# locations
ZLBDIR = zlib
DFLDIR = libdeflate
HTSDIR = htslib
SAMDIR = samtools
UTLDIR = utils

SCRIPTDIR := ../scripts
PRTNAME := $(shell $(SCRIPTDIR)/guessport)
PRTDIR = ../ports/$(PRTNAME)

# programs
HTSPRG = bgzip htsfile tabix
SAMPRG = samtools

# flags for configuring htslib and samtools

CONF_HSAM_FLAGS = CPPFLAGS='-I../libdeflate -I../zlib' \
                  LDFLAGS='-L../libdeflate -L../zlib -Wl,-L../libdeflate -L../zlib'

CONF_HSAM_FLAGS += --disable-plugins --disable-libcurl

ifndef WITH_BZ2
CONF_HSAM_FLAGS += --disable-bz2
endif

ifndef WITH_LZMA
CONF_HSAM_FLAGS += --disable-lzma
endif

# flags specific to samtools

ifndef WITH_CURSES
CONF_OSAM_FLAGS += --without-curses --without-ncursesw
endif


# -------------------------------------
# rules
#

.PHONY: all banner footer zlblib dfllib htslib sam utils
.PHONY: instdir install test dev_test clean distclean
.PHONY: zlblib_conf htslib_conf sam_conf

all: banner zlblib dfllib htslib sam utils footer

banner:
	@echo '[0;34m'
	@echo "----------------------------------------"
	@echo " make WGInR C sources"
	@echo "----------------------------------------"
	@echo '[m'

footer:
	@echo '[0;32m'
	@echo "----------------------------------------"
	@echo " WGInR C sources ok"
	@echo "----------------------------------------"
	@echo '[m'

zlblib_conf:
	(test -e $(ZLBDIR)/zlib.pc) || (cd $(ZLBDIR) && ./configure --static)

zlblib: zlblib_conf
	cd $(ZLBDIR) && $(MAKE)

dfllib:
	cd $(DFLDIR) && make CFLAGS='-fPIC -O3' libdeflate.a

htslib_conf:
	(test -e $(HTSDIR)/config.h) || (cd $(HTSDIR) && ./configure $(CONF_HSAM_FLAGS))
	
htslib: dfllib htslib_conf
	cd $(HTSDIR) && $(MAKE)

sam_conf:
	(test -e $(SAMDIR)/config.h) || (cd $(SAMDIR) && ./configure $(CONF_HSAM_FLAGS) $(CONF_OSAM_FLAGS))

sam: htslib sam_conf
	cd $(SAMDIR) && $(MAKE)

utils: sam
	cd $(UTLDIR) && $(MAKE)
    
instdir:
	-@(test -d $(PRTDIR))     || ($(MKDIR) $(PRTDIR))
	-@(test -d $(PRTDIR)/bin) || ($(MKDIR) $(PRTDIR)/bin)

install: instdir
	cd $(HTSDIR) && $(CP) $(HTSPRG) ../$(PRTDIR)/bin
	cd $(SAMDIR) && $(CP) $(SAMPRG) ../$(PRTDIR)/bin
	cd $(UTLDIR) && make install

test:
	cd $(UTLDIR) && make test
	@echo "other tests are available as make dev_test"

dev_test:
	-cd $(ZLBDIR) && make test
	-cd $(HTSDIR) && make test
	-cd $(SAMDIR) && make test

clean:
	-cd $(ZLBDIR) && make clean
	-cd $(DFLDIR) && make clean
	-cd $(HTSDIR) && make clean
	-cd $(SAMDIR) && make clean
	-cd $(UTLDIR) && make clean

distclean: clean
	-cd $(ZLBDIR) && make distclean
	-cd $(HTSDIR) && make distclean
	-cd $(SAMDIR) && make distclean
	-cd $(UTLDIR) && make distclean
