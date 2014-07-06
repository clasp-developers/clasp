
include local.config



ifneq ($(EXTERNALS),)
	PATH := $(EXTERNALS)/release/bin:$(EXTERNALS)/common/bin:$(PATH)
	export PATH
endif


ifeq ($(WHAT),)
	WHAT = bundle release boehm
endif

all:
	echo PJOBS=$(PJOBS)
	echo TARGET-OS=$(TARGET-OS)
	echo LINK=$(LINK)
	(cd src/main;  bjam -j$(PJOBS) target-os=$(TARGET-OS) link=$(LINK) $(WHAT))
	make compile-sources

compile-sources:
	(cd src/main; make system-boot)

clean:
	(cd src/main; rm -rf bin)
	(cd src/core; rm -rf bin)
	(cd src/gctools; rm -rf bin)
	(cd src/llvmo; rm -rf bin)
