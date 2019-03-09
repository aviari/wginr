#
# $Id: Makefile 474 2019-01-10 23:38:37Z viari $
#

# -------------------------------------
# some standard commands
#

SHELL = /bin/sh

RM    = rm

CP    = cp

RCMD  = R CMD

RCHECK = ./etc/checkinstall

RTEST = ./etc/testunit

RINST = ./etc/bootinstall

# -------------------------------------
# locations
#

SCRIPTDIR = ../scripts

PRTNAME := $(shell $(SCRIPTDIR)/guessport)

# -------------------------------------
# library sources
#

PACK_BOOT = lx

PACK_UTIL = rbgzf thmm xlx asdog

PACK_SRC = $(PACK_BOOT:=.src) $(PACK_UTIL:=.src)

PACK_MAN = $(PACK_BOOT:=.man) $(PACK_UTIL:=.man)

PACK_INS = $(PACK_UTIL:=.ins)

CRAN_REPOS = http://cran.irsn.fr

# -------------------------------------
# implicit rules
#

%.src: %
	$(RCMD) build $<
	\mv -f $(<)_?.?.tar.gz packages/source

%.man: %
	$(RCMD) Rd2pdf --title="$< Package" --batch --no-preview --force -o $</inst/doc/$<.pdf $<

%.ins: %
	$(RINST) $<

# -------------------------------------
# rules
#

.PHONY: all banner footer packages manuals doc install depends
.PHONY: instdir instdir.chk instdir.fail
.PHONY: testunit test check clean distclean
.PHONY: $(PACK_BOOT) $(PACK_UTIL)
.PHONY: dev.chk dev.rd dev.doc dev.test dev.bin dev.stdbin dev.clean

all: banner packages footer

banner:
	@echo '[0;34m'
	@echo "----------------------------------------"
	@echo " make WGInR R sources"
	@echo "----------------------------------------"
	@echo '[m'

footer:
	@echo '[0;32m'
	@echo "----------------------------------------"
	@echo " WGInR R sources ok"
	@echo "----------------------------------------"
	@echo '[m'

packages: $(PACK_SRC)

manuals: $(PACK_MAN)

doc: manuals

install: instdir $(PACK_BOOT).ins depends $(PACK_INS)
	@echo "----------------------------------------"
	@echo "installing R scripts in $(SCRIPTDIR)"
	@echo "----------------------------------------"
	-$(CP) -fp scripts/*.r $(SCRIPTDIR)

depends:
	-$(RCHECK) --package=xtable        --repos=$(CRAN_REPOS)
	-$(RCHECK) --package=bit           --repos=$(CRAN_REPOS)
	-$(RCHECK) --package=intervals     --repos=$(CRAN_REPOS) --type="source"
	-$(RCHECK) --package=digest        --repos=$(CRAN_REPOS) --type="source"
	-$(RCHECK) --package=nlme          --repos=$(CRAN_REPOS)
	-$(RCHECK) --package=mgcv          --repos=$(CRAN_REPOS)
	-$(RCHECK) --package=KFAS          --repos=$(CRAN_REPOS)
	-$(RCHECK) --package=Rcpp          --repos=$(CRAN_REPOS)
	-$(RCHECK) --package=RcppArmadillo --repos=$(CRAN_REPOS)
	-$(RCHECK) --package=bcp           --repos=$(CRAN_REPOS)
	-$(RCHECK) --package=data.table    --repos=$(CRAN_REPOS)
	-$(RCHECK) --package=RColorBrewer  --repos=$(CRAN_REPOS)
	-$(RCHECK) --package=viridisLite   --repos=$(CRAN_REPOS)
	-$(RCHECK) --package=testthat      --repos=$(CRAN_REPOS) # for development

instdir:
	make instdir.chk || make instdir.fail

instdir.chk:
	@-(! test $${R_LIBS}) || (test -d $${R_LIBS}) || mkdir $${R_LIBS}
	$(eval R_DIR = $(shell etc/bootpath))
	@touch $(R_DIR)/_$$$$ && \rm -f $(R_DIR)/_$$$$
	@echo " R install dir is : $(R_DIR)"

instdir.fail:
	$(eval R_DIR = $(shell etc/bootpath))
	@echo '[0;31m'
	@echo ' ======================================='
	@echo " R install dir is : $(R_DIR)"
	@echo ' ======================================='
	@echo ' You probably do not have write access to this location'
	@echo ' Set the environment variable R_LIBS to $$home/R'
	@echo '    (you may choose any other name)'
	@echo '    csh : setenv R_LIBS $$home/R'
	@echo '   bash : export R_LIBS=$$HOME/R'
	@echo ' Create this directory if necessary'
	@echo ' and rerun "make install"'
	@echo ' ======================================='
	@echo '[m'
	
testunit:
	$(RTEST) --package=lx
	$(RTEST) --package=thmm

test: testunit
	cd tests && make test

check:
	$(RCMD) build lx && $(RCMD) check lx_*.tar.gz && $(RM) -f lx_*.tar.gz
	$(RCMD) build rbgzf && $(RCMD) check rbgzf_*.tar.gz && $(RM) -f rbgzf_*.tar.gz
	$(RCMD) build thmm && $(RCMD) check thmm_*.tar.gz && $(RM) -f thmm_*.tar.gz
	$(RCMD) build xlx && $(RCMD) check xlx_*.tar.gz && $(RM) -f xlx_*.tar.gz

clean:
	cd tests && make clean
	-\rm -r -f rbgzf/src/*.o rbgzf/src/*.so 
	-\rm -r -f thmm/src/*.o thmm/src/*.so 
	-\rm -r -f .Rd2pdf* *.Rcheck lx/inst/tests/Rplots.* 

distclean: clean
##	-$(RM) -f $(wildcard packages/source/lx_*.tar.gz)
##	-$(RM) -f $(wildcard packages/source/xlx_*.tar.gz)

# -------------------------------------
# rules for development only
#
# require devtools, roxygen2
#

dev.chk:
	$(if $(value TARGET),, $(error use with TARGET=<package>))

dev.rd:  dev.chk                        # make dev.rd TARGET=<package>
	cd $(TARGET)  && R -e 'library(devtools);document()'

dev.doc: dev.chk dev.rd                 # make dev.doc TARGET=<package>
	$(RCMD) Rd2pdf --title="$(TARGET) Package" --no-preview --force -o $(TARGET)/inst/doc/$(TARGET).pdf $(TARGET)

dev.check: dev.chk                      # make dev.check TARGET=<package>
	$(RCMD) build $(TARGET) && $(RCMD) check $(TARGET)_*.tar.gz && $(RM) -f $(TARGET)_*.tar.gz

dev.test: dev.chk                      # make dev.test TARGET=<package>
	$(RTEST) --package=$(TARGET)

dev.bin: dev.chk                      # make dev.bin TARGET=<package>
	$(RCMD) INSTALL --build $(TARGET)

#
# some packages maybe difficult to recompile
# so we provide binaries for them
# this target will produce these binary packages
#
# don't forget to install them in the proper place
# "packages/binary/PORTNAME" and to link them to proper
# name "pack.tar.gz"
#
dev.stdbin:
	make dev.bin TARGET=packages/source/Rcpp.tar.gz
	make dev.bin TARGET=packages/source/RcppArmadillo.tar.gz
	make dev.bin TARGET=packages/source/bcp.tar.gz
	@echo "install them into packages/binary/$(PRTNAME)"
	
dev.clean:   # uninstall packages
	./etc/devclean lx xlx rbgzf thmm bit intervals digest testthat
