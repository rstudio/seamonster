## This Makefile automates common tasks in R package developement
## Copyright (C) 2013 Renaud Gaujoux

#%AUTHOR_USER%#
#%R_PACKAGE%#
#%R_PACKAGE_PROJECT_PATH%#
#%R_PACKAGE_PROJECT%#
#%R_PACKAGE_SUBPROJECT_PATH_PART%#
#%R_PACKAGE_OS%#
#%R_PACKAGE_PATH%#
#%R_PACKAGE_TAR_GZ%#

# auto-conf variables
#%INIT_CHECKS%#
#

ifndef R_PACKAGE
#$(error Required make variable 'R_PACKAGE' is not defined.)
endif
ifndef R_PACKAGE_PATH
R_PACKAGE_PATH=../pkg
endif

#ifndef R_LIBS
R_LIBS=#%R_LIBS%#
#endif

RSCRIPT_DEVEL=Rscript-devel

ifdef devel
flavour=devel
RSCRIPT=$(RSCRIPT_DEVEL)
endif

ifdef flavour
RCMD=R$(flavour)
RSCRIPT=Rscript-$(flavour)
CHECK_DIR=checks/$(flavour)
else
CHECK_DIR=checks
endif

R_BIN=#%R_BIN%#

ifndef RSCRIPT
RSCRIPT:=$(R_BIN)/Rscript
endif
ifndef RCMD
RCMD:=$(R_BIN)/R
endif

QUICK_FLAG=FALSE
ifdef quick
QUICK_FLAG=TRUE
quick_build=TRUE
R_CHECK_ARGS=--no-tests --no-vignettes
endif

ifdef quick_build
R_BUILD_ARGS=--no-build-vignettes
endif

ifdef full
_R_LOCAL_CHECK_=true
endif

## BUILD-BINARIES COMMAND
#define CMD_BUILD_BINARIES
#library(devtools);
#library(methods);
#p <- as.package('$(R_PACKAGE_PATH)');
#pdir <- p[['path']];
#src <- paste0(p[['package']], '_', p[['version']], '.tar.gz')
#run <- function(){
#tmp <- tempfile()
#on.exit( unlink(tmp, recursive=TRUE) )
#cmd <- paste0("wine R CMD INSTALL -l ", tmp, ' --build ', src)
#message("R CMD check command:\n", cmd)
#system(cmd, intern=FALSE, ignore.stderr=FALSE)
#}
#run()
#endef

define package_info
	@echo -n "# R version: "
	@echo -n `$(RSCRIPT) --version`
	# R Platform: $(R_PACKAGE_OS)
	# Project: $(R_PACKAGE_PROJECT)
	# Package: $(R_PACKAGE) version $(R_PACKAGE_VERSION)
	# Project directory: '$(R_PACKAGE_PROJECT_PATH)'
	# Project sub-directory: '$(R_PACKAGE_SUBPROJECT_PATH_PART)'
	# Package directory: '$(R_PACKAGE_PATH)'
endef

all: roxygen build check 

dist: all staticdoc

init: | $(CHECK_DIR)
	$(package_info)

$(CHECK_DIR):
	mkdir -p $(CHECK_DIR)

info: | $(R_PACKAGE_PATH)
	$(package_info)

ifdef R_PACKAGE_HAS_VIGNETTES
ifndef quick
build: vignettes
else
build: init
endif
else
build: init
endif
	@cd $(CHECK_DIR) && \
	echo "\n*** STEP: BUILD\n" && \
	$(RCMD) CMD build $(R_BUILD_ARGS) "$(R_PACKAGE_PATH)" && \
	echo "*** DONE: BUILD"

deploy: info
	@echo "\n*** STEP: DEPLOY (R-CURRENT)\n" && \
	$(RSCRIPT) -e "devtools::install('$(R_PACKAGE_PATH)', quick = $(QUICK_FLAG))" && \
	echo "\n*** DONE: DEPLOY (R-CURRENT)"
	
deploy-all: deploy
	@echo "\n*** STEP: DEPLOY (R-DEVEL)" && \
	echo `$(RSCRIPT_DEVEL) --version` && \
	$(RSCRIPT_DEVEL) -e "devtools::install('$(R_PACKAGE_PATH)', quick = $(QUICK_FLAG))" && \
	echo "\n*** DONE: DEPLOY (R-DEVEL)"

deploy-repo: build $(CHECK_DIR)/$(R_PACKAGE_TAR_GZ)
	@cd $(CHECK_DIR) && \
	echo "\n*** STEP: DEPLOY-REPO\n" && \
	cp $(R_PACKAGE_TAR_GZ) ~/projects/CRANx/src/contrib && \
	echo "*** DONE: DEPLOY-REPO"
	
#build-bin: build
#	@cd $(CHECK_DIR) && \
#	echo "\n*** STEP: BUILD-BINARIES\n" && \
#	`echo "$$CMD_BUILD_BINARIES" > build-bin.r` && \
#	$(RSCRIPT) --vanilla ./build-bin.r && \
#	echo "\n*** DONE: BUILD-BINARIES"
	
check: build $(CHECK_DIR)/$(R_PACKAGE_TAR_GZ)
	@cd $(CHECK_DIR) && \
	echo "\n*** STEP: CHECK\n" && \
	mkdir -p $(R_PACKAGE_OS) && \
	$(R_LIBS) $(RCMD) CMD check $(R_CHECK_ARGS) -o $(R_PACKAGE_OS) --as-cran --timings $(R_PACKAGE_TAR_GZ) && \
	echo "*** DONE: CHECK"

roxygen: init
	@cd $(CHECK_DIR) && \
	echo "\n*** STEP: ROXYGEN\n" && \
	roxy $(R_PACKAGE_PATH) && \
	echo "\n*** DONE: ROXYGEN"

staticdocs: init
	echo "\n*** STEP: STATICDOCS\n" && \
	Rstaticdocs $(R_PACKAGE) $(target) && \
	echo "\n*** DONE: STATICDOCS\n"

ifdef R_PACKAGE_HAS_VIGNETTES
ifdef rebuild
vignettes: init rmvignettes
else
vignettes: init
endif
	@cd $(CHECK_DIR) && \
	cd $(R_PACKAGE_PATH)/vignettes && \
	echo "\n*** STEP: BUILD VIGNETTES\n" && \
	$(eval VIGNETTE_MK := $(shell cd "$(R_PACKAGE_PATH)/vignettes"; $(RSCRIPT) --vanilla -e "pkgmaker::vignetteMakefile('$(R_PACKAGE)', checkMode = FALSE)")) \
	cd "$(R_PACKAGE_PATH)/vignettes" && \
	make -f $(VIGNETTE_MK) && \
	echo "Cleaning up ..." && \
	make -f $(VIGNETTE_MK) clean && \
	echo "\n*** DONE: BUILD VIGNETTES\n"

rmvignettes: init
	@cd $(CHECK_DIR) && \
	cd $(R_PACKAGE_PATH)/vignettes && \
	echo "\n*** STEP: REMOVE VIGNETTES\n" && \
	$(eval VIGNETTE_MK := $(shell cd "$(R_PACKAGE_PATH)/vignettes"; $(RSCRIPT) --vanilla -e "pkgmaker::vignetteMakefile('$(R_PACKAGE)', checkMode = FALSE)")) \
	cd "$(R_PACKAGE_PATH)/vignettes" && \
	make -f $(VIGNETTE_MK) clean-all && \
	echo "\n*** DONE: REMOVE VIGNETTES\n"
	
cp-vignettes: init
	@cd $(CHECK_DIR) && \
	cd $(R_PACKAGE_PATH)/vignettes && \
	echo "\n*** STEP: COPYING VIGNETTE FILES TO inst/doc\n" && \
	mkdir -p ../inst/doc && \
	cp -f *.Rnw ../inst/doc && \
	echo "\n*** DONE: COPYING VIGNETTES FILES\n"
endif

r-forge: build
	@cd $(CHECK_DIR) && \
	echo "\n*** STEP: R-FORGE" && \
	echo -n "  - package source ... " && \
	tar xzf $(R_PACKAGE_TAR_GZ) -C ../r-forge/pkg$(R_PACKAGE_SUBPROJECT_PATH_PART)/ --strip 1 $(R_PACKAGE) && \
	echo "OK" && \
	echo -n "  - static doc ... " && \
	rsync --delete --recursive --cvs-exclude $(R_PACKAGE_PROJECT_PATH)/www$(R_PACKAGE_SUBPROJECT_PATH_PART)/ ../r-forge/www$(R_PACKAGE_SUBPROJECT_PATH_PART)/ && \
	echo "OK" && \
	echo "*** DONE: R-FORGE\n"
	
myCRAN: build
	@cd $(CHECK_DIR) && \
	echo "\n*** STEP: myCRAN" && \
	echo -n "  - package source ... " && \
	cp $(R_PACKAGE_TAR_GZ) ~/projects/myCRAN/src/contrib && \
	echo "OK" && \
	echo "  - update index ... " && \
	cd ~/projects/myCRAN/ && ./update && cd - && \
	echo "  - update staticdocs ... " && \
	cd ~/projects/myCRAN/ && rsync --delete --recursive --cvs-exclude $(R_PACKAGE_PROJECT_PATH)/www$(R_PACKAGE_SUBPROJECT_PATH_PART)/ web/$(R_PACKAGE)/ && \
	echo "DONE: index" && \
	echo "*** DONE: myCRAN\n"

newdoc:
	
	@echo "Generating new document: $(title)"
	if [ ! -d "$(R_PACKAGE_PATH)/inst/examples/_src" ]; then \
		mkdir -p "$(R_PACKAGE_PATH)/inst/examples/_src" \
	fi \
	if [ -e "$(title)" ]; then \
		echo "Missing title" \
		exit 1 \
	fi \
	echo "---\nlayout: post\ntags: [$(R_PACKAGE) $(tags)]\ncategories: $(categories)\ntitle: $(title)\n---" \
	> "$(R_PACKAGE_PATH)/inst/examples/_src/`date +%F`-$(title).Rmd"
