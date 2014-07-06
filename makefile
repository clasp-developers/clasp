
include local.config



ifeq ($(EXTERNALS),)
	export BJAM = bjam
else
	export BJAM = $(EXTERNALS)/release/bin/bjam
	PATH := $(EXTERNALS)/release/bin:$(EXTERNALS)/common/bin:$(PATH)
	export PATH
endif


ifeq ($(WHAT),)
	WHAT = bundle release boehm
endif

all:
	echo bjam = `file $(BJAM)`
	(cd src/main;  $(BJAM) -j$(PJOBS) target-os=$(TARGET-OS) link=$(LINK) $(WHAT))
	make compile-sources

compile-sources:
	(cd src/main; make system-boot)

clean:
	(cd src/main; rm -rf bin)
	(cd src/core; rm -rf bin)
	(cd src/gctools; rm -rf bin)
	(cd src/llvmo; rm -rf bin)