
include local.config

BOOST_BUILD_V2_SOURCE_DIR = boost_build_v2
BOOST_BUILD_V2_INSTALL = $(PREFIX)/Contents/boost_build_v2
BJAM = $(BOOST_BUILD_V2_INSTALL)/bin/bjam

ifneq ($(EXTERNALS),)
	PATH := $(PATH):$(EXTERNALS)/release/bin:$(EXTERNALS)/common/bin
	export PATH
endif


ifeq ($(WHAT),)
	WHAT = bundle debug release boehm
endif

all:
	make boostbuildv2-build
	make clasp-build
	make compile-sources

testing:
	which clang++

clasp-build:
	(cd src/main; $(BJAM) -j$(PJOBS) target-os=$(TARGET-OS) link=$(LINK) $(WHAT))

boostbuildv2-build:
	(cd $(BOOST_BUILD_V2_SOURCE_DIR); ./bootstrap.sh; ./b2 toolset=clang install --prefix=$(BOOST_BUILD_V2_INSTALL))

compile-sources:
	(cd src/main; make system-boot)

clean:
	(cd src/main; rm -rf bin)
	(cd src/core; rm -rf bin)
	(cd src/gctools; rm -rf bin)
	(cd src/llvmo; rm -rf bin)
