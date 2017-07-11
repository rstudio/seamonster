## This Makefile was inspired from the RcppGSL package
## Copyright (C) 2011 Romain François and Edd Eddelbuettel
## It was modifed by Renaud Gaujoux to make it more generic and to generate the 
## fake vignettes on the fly.
## Copyright (C) 2011 Renaud Gaujoux

## There is an old bug in texidvi that makes it not swallow the ~
## marker used to denote whitespace. This is actually due to fixing
## another bug whereby you could not run texidvi on directory names
## containing a tilde (as we happen to do for Debian builds of R
## alpha/beta/rc releases). The 'tilde' bug will go away as it
## reportedly has been squashed upstream but I am still bitten by it
## on Ubuntu so for now Dirk will insist on pdflatex and this helps.

#%AUTHOR_USER%#
#%MAKE_R_PACKAGE%#

ifndef MAKE_R_PACKAGE
$(error Required make variable 'MAKE_R_PACKAGE' is not defined.)
endif
#ifndef AUTHOR_USER
#$(error Required make variable 'AUTHOR_USER' is not defined.)
#endif
ifndef MAKEPDF
MAKEPDF=1
endif

##---------------------------------------------------------------------
## Everything below this should be generic and work for any package provided that
## they have the following directory inst/doc setting:
## - inst/vignettes/src: contains the Rnw files for normal vignettes
## - tests: contains R code to run unit tests that are run if not checking and
## produce the file <package>-unitTests.pdf  
##---------------------------------------------------------------------

SRC_DIR=.
RNW_SRCS = #%RNW_SRCS%#
PDF_OBJS=$(RNW_SRCS:.Rnw=.pdf)
# allow redfining pdf targets in local mode
#%PDF_OBJS%#

TEX_OBJS=$(RNW_SRCS:.Rnw=.tex)

ifneq (${R_HOME},)
R_CHECK=1
else
R_CHECK=0

# Enabling local mode?
#%LOCAL_MODE%#

# in local mode: use pdflatex
ifdef LOCAL_MODE
USE_PDFLATEX=1
endif

export MAKE_R_PACKAGE

ifdef LOCAL_MODE
MAKEPDF=1
endif

# Type of pre-install:
# for back-compatibility
ifdef QUICK
quick=1
endif

ifdef quick
install=quick
endif
ifndef install
install=yes
endif

ifneq ('$(install)', 'no')
ifeq ('$(install)','yes')
# install in temporary directory at each run
TMP_INSTALL_DIR:=#%TMP_INSTALL_DIR%#
FORCE_INSTALL:=TRUE
else
ifeq ('$(install)','quick')
QUICK=1
FORCE_INSTALL:=FALSE
TMP_INSTALL_DIR:=tmplib
endif
endif

# export R_LIBS
export R_LIBS:=#%R_LIBS_DEV%#
endif # end install pkg

endif #end not R_CHECK


# Define command for temporary installation (used when make is directly called,
# i.e. when not in check/build/INSTALL)
ifdef TMP_INSTALL_DIR
define do_install  
  @if [ ! -d "$(TMP_INSTALL_DIR)/$(MAKE_R_PACKAGE)" ]; then \
	echo "# Installing package '$(MAKE_R_PACKAGE)' in '$(TMP_INSTALL_DIR)' "; \
	$(RSCRIPT) --vanilla --quiet -e "pkgmaker::quickinstall('..', '$(TMP_INSTALL_DIR)')" > Rinstall.log 2> Rinstall.err; \
	if [ ! -d "$(TMP_INSTALL_DIR)/$(MAKE_R_PACKAGE)" ]; then \
		echo "ERROR: Temporary installation failed: see files Rinstall.log and Rinstall.err"; \
		echo "# Removing temporary library directory $(TMP_INSTALL_DIR)"; \
		exit 1; \
	else \
		echo "# Package successfully installed"; \
	fi \
  fi
endef
else
define do_install
endef	
endif

define showInfo
  @echo "# Using R home: $(R_HOME)"
  @echo "# Using R architecture: $(R_ARCH_BIN)"
  @echo "# Using R bin directory: $(R_BIN)"
  @echo "# Using R_LIBS: $(R_LIBS)"
endef

#%INST_TARGET%#
#ifdef INST_TARGET
define update_inst_doc
	# Copying vignette source and output files to ../inst/doc
	mkdir -p ../inst/doc
	#cp -f $1 ../inst/doc
	mv -f $2 ../inst/doc
endef
#else
#define update_inst_doc
#	# Copying vignette output files to ../inst/doc
#	mkdir -p ../inst/doc
#	cp -f $2 ../inst/doc
#endef	
#endif

all: init $(PDF_OBJS) do_clean
	@echo "# All vignettes in 'vignettes' are up to date"

init:
	# Generating vignettes for package '$(MAKE_R_PACKAGE)'
	# DESCRIPTION file in: #%R_PACKAGE_DESCRIPTION%# 
	# User: #%VIGNETTE_USER%#
	# Maintainer(s): #%VIGNETTE_MAINTAINERS%#
	$(showInfo)

ifdef LOCAL_MODE
	# Mode: Local Development [$(LOCAL_MODE)]
else
	# Mode: Production
endif
ifneq ($(R_CHECK),0)
	# R CMD check: TRUE
else
	# R CMD check: FALSE
endif
ifdef INST_TARGET
	# BuildVignettes: no (storing in ../inst/doc) 
endif
	# Detected vignettes: $(RNW_SRCS)
	# Detected targets: $(PDF_OBJS)

clean:
	rm -fr *.bbl *.run.xml *.blg *.aux *.out *-blx.bib \
	*.log *.err Rplots.pdf tests-results tmplib vignette_*.mk vignette.mk \
	cleveref.sty \
	runit.* 
ifndef LOCAL_MODE
	rm -f $(TEX_OBJS)
endif

clean-all: clean
	rm -fr $(TEX_OBJS) $(PDF_OBJS) $(MAKE_R_PACKAGE)-unitTests.Rnw

setvars:
ifeq (${R_BIN},)
R_BIN=#%R_BIN%#
endif
RPROG:=$(R_BIN)/R
RSCRIPT:=$(R_BIN)/Rscript

.SECONDARY: %.tex

do_clean:
ifndef QUICK
	# Removing temporary install directory '$(TMP_INSTALL_DIR)'
	@-rm -rf $(TMP_INSTALL_DIR);
endif


# only run tests if not checking: CRAN check run the tests separately
#ifdef INST_TARGET
../inst/doc/%-unitTests.pdf:
#else
#%-unitTests.pdf:
#endif
	# Generating vignette for unit tests: $@
	$(do_install)
	$(RSCRIPT) --vanilla -e "pkgmaker::makeUnitVignette('package:$(MAKE_R_PACKAGE)', check=$(R_CHECK))" >> unitTests.log
ifdef LOCAL_MODE
	$(eval VIGNETTE_BASENAME := $(shell basename $@ .pdf))
	# Compact vignette file
	$(RSCRIPT) --vanilla -e "pkgmaker::compactVignettes('$(VIGNETTE_BASENAME).pdf')"
endif
	$(call update_inst_doc, $*-unitTests.Rnw, $*-unitTests.pdf)


# Generate .pdf from .Rnw
#ifdef INST_TARGET
../inst/doc/%.pdf: ${SRC_DIR}/%.Rnw
#else
#%.pdf: ${SRC_DIR}/%.Rnw
#endif
	# Generating vignette $@ from ${SRC_DIR}/$*.Rnw
	$(do_install)
	# Compiling ${SRC_DIR}/$*.Rnw into $*.tex
	$(RSCRIPT) --vanilla -e "pkgmaker::rnw('${SRC_DIR}/$*.Rnw', '$*.tex');"
	# Generating pdf $@ from $*.tex
ifdef MAKEPDF
ifdef USE_PDFLATEX
	# Using pdflatex
	# LaTeX compilation 1/3
	@pdflatex $* >> $*-pdflatex.log
	# Compiling bibliography with bibtex
	-bibtex $*
	# LaTeX compilation 2/3
	@pdflatex $* >> $*-pdflatex.log
	# LaTeX compilation 3/3
	@pdflatex $* >> $*-pdflatex.log
	# Compact vignettes
	$(RSCRIPT) --vanilla -e "pkgmaker::compactVignettes('$*.pdf')"
	# Remove temporary LaTeX files (but keep the .tex)
	rm -fr $*.toc $*.log $*.bbl $*.blg $*.aux $*.out $*-blx.bib	
	
else
	# Using tools::texi2dvi
	# LaTeX compilation 1/2
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( '$*.tex', pdf = TRUE, clean = FALSE )"
	# Compiling bibliography with bibtex
	-bibtex $*
	# LaTeX compilation 2/2
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( '$*.tex', pdf = TRUE, clean = TRUE )"
endif
endif	
	# Update fake vignette file ./$*.Rnw
	$(RSCRIPT) --vanilla -e "pkgmaker::makeFakeVignette('${SRC_DIR}/$*.Rnw', '$*.Rnw')"
	$(call update_inst_doc, $*.Rnw, $*.pdf)
