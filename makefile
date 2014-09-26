include local.config

BOOST_BUILD_V2_SOURCE_DIR = boost_build_v2
BOOST_BUILD_V2_INSTALL = $(CLASP_BUILD_TARGET_DIR)/Contents/boost_build_v2
BJAM = $(BOOST_BUILD_V2_INSTALL)/bin/bjam
export CLASP_APP_RESOURCES_DIR = $(CLASP_BUILD_TARGET_DIR)/Contents/Resources

ifneq ($(EXTERNALS),)
	PATH := $(PATH):$(EXTERNALS)/release/bin:$(EXTERNALS)/common/bin
	export PATH
endif


ifeq ($(WHAT),)
	WHAT = bundle debug release boehm mps
endif

all:
	make boostbuildv2-build
	make compile-commands
	make clasp-boehm
	make clasp-mps


testing:
	which clang++

clasp-mps:
	(cd src/main; $(BJAM) -j$(PJOBS) link=$(LINK) bundle release mps)
	(cd src/main; make mps)

clasp-mps2:
	(cd src/main; $(BJAM) -j$(PJOBS) target-os=$(TARGET_OS) link=$(LINK) bundle release mps)
	(cd src/main; make mps)

clasp-boehm2:
	(cd src/main; $(BJAM) -j$(PJOBS) target-os=$(TARGET_OS) link=$(LINK) bundle release boehm)
	(cd src/main; make boehm)

clasp-boehm:
	(cd src/main; $(BJAM) -j$(PJOBS) link=$(LINK) bundle release boehm)
	(cd src/main; make boehm)


boostbuildv2-build:
	(cd $(BOOST_BUILD_V2_SOURCE_DIR); ./bootstrap.sh; ./b2 toolset=clang install --prefix=$(BOOST_BUILD_V2_INSTALL))

compile-commands:
	(cd src/main; make compile-commands)


clean:
	(cd src/main; rm -rf bin bundle)
	(cd src/core; rm -rf bin bundle)
	(cd src/gctools; rm -rf bin bundle)
	(cd src/llvmo; rm -rf bin bundle)
	(cd src/cffi; rm -rf bin bundle)
	(cd src/clbind; rm -rf bin bundle)
	(cd src/sockets; rm -rf bin bundle)
	(cd src/serveEvent; rm -rf bin bundle)
ifneq ($(CLASP_BUILD_TARGET_DIR),)
	(cd $(CLASP_BUILD_TARGET_DIR); rm -rf *)
endif
