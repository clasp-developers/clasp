system	 	:= "asdf"
webhome_private := common-lisp.net:/project/asdf/public_html/
webhome_public	:= "http://common-lisp.net/project/asdf/"
clnet_home      := "/project/asdf/public_html/"
sourceDirectory := $(shell pwd)

#### Common Lisp implementations available for testing.
## export ASDF_TEST_LISPS to override the default list of such implementations,
## or specify a lisps= argument at the make command-line
defaultLisps = ccl clisp sbcl ecl ecl_bytecodes cmucl abcl scl allegro lispworks allegromodern gcl xcl mkcl
ifdef ASDF_TEST_LISPS
lisps ?= ${ASDF_TEST_LISPS}
else
lisps ?= ${defaultLisps}
endif
ifdef ASDF_UPGRADE_TEST_LISPS
  ulisps ?= ${ASDF_UPGRADE_TEST_LISPS}
else
  ifdef ASDF_TEST_LISPS
    ulisps ?= ${ASDF_TEST_LISPS}
  else
    ulisps ?= ${defaultLisps}
  endif
endif


## grep for #+/#- features in the test/ directory to see plenty of disabled tests on some platforms
## NOT SUPPORTED BY OUR AUTOMATED TESTS:
##	cormancl genera lispworks-personal-edition rmcl
## Some are manually tested once in a while.
ifdef ASDF_TEST_SYSTEMS
s ?= ${ASDF_TEST_SYSTEMS}
endif

ifdef ASDF_DEVEL_SOURCE_REGISTRY
export CL_SOURCE_REGISTRY = ${ASDF_DEVEL_SOURCE_REGISTRY}
endif

l ?= sbcl

ABCL ?= abcl
ALLEGRO ?= alisp
ALLEGROMODERN ?= mlisp
CCL ?= ccl
CLISP ?= clisp
CMUCL ?= cmucl
ECL ?= ecl
GCL ?= gcl
LISPWORKS ?= lispworks
MKCL ?= mkcl
SBCL ?= sbcl
SCL ?= scl
XCL ?= xcl

header_lisp := header.lisp
driver_lisp := uiop/package.lisp uiop/common-lisp.lisp uiop/utility.lisp uiop/os.lisp uiop/pathname.lisp uiop/filesystem.lisp uiop/stream.lisp uiop/image.lisp uiop/run-program.lisp uiop/lisp-build.lisp uiop/configuration.lisp uiop/backward-driver.lisp uiop/driver.lisp
defsystem_lisp := upgrade.lisp component.lisp system.lisp cache.lisp find-system.lisp find-component.lisp operation.lisp action.lisp lisp-action.lisp plan.lisp operate.lisp output-translations.lisp source-registry.lisp backward-internals.lisp parse-defsystem.lisp bundle.lisp concatenate-source.lisp backward-interface.lisp package-inferred-system.lisp interface.lisp user.lisp footer.lisp
all_lisp := $(header_lisp) $(driver_lisp) $(defsystem_lisp)

# Making ASDF itself should be our first, default, target:
build/asdf.lisp: $(all_lisp)
	mkdir -p build
	rm -f $@
	cat $(all_lisp) > $@

# This quickly locates such mistakes as unbalanced parentheses:
load: build/asdf.lisp
	./test/run-tests.sh -t $l $(all_lisp)

install: archive

bump: bump-version
	git commit -a -m "Bump version to $$(eval a=$$(cat version.lisp-expr) ; echo $$a)"
	temp=$$(cat version.lisp-expr); temp="$${temp%\"}"; temp="$${temp#\"}"; git tag $$temp

bump-version: build/asdf.lisp
	./bin/asdf-builder bump-version ${v}

driver-files:
	@echo $(driver_lisp)

defsystem-files:
	@echo $(defsystem_lisp)

archive: build/asdf.lisp
	./bin/asdf-builder make-and-publish-archive

### Count lines separately for asdf-driver and asdf itself:
wc:
	@wc $(driver_lisp) | sort -n ; echo ; \
	wc $(header_lisp) $(defsystem_lisp) | sort -n ; \
	echo ; \
	wc $(header_lisp) $(driver_lisp) $(defsystem_lisp) | tail -n 1

push:
	git status
	git push --tags cl.net release master
	git push --tags github release master
	git fetch
	git status

doc:
	${MAKE} -C doc

website:
	${MAKE} -C doc website

clean_dirs = $(sourceDirectory)
clean_extensions = fasl dfsl cfsl fasl fas lib dx32fsl lx64fsl lx32fsl ufasl o bak x86f vbin amd64f sparcf sparc64f hpf hp64f

clean:
	@for dir in $(clean_dirs); do \
	     if test -d $$dir; then \
	     	 echo Cleaning $$dir; \
		 for ext in $(clean_extensions); do \
		     find $$dir \( -name "*.$$ext" \) \
	     	    -and -not -path \""*/.git/*"\" \
		     	  -and -not -path \""*/_darcs/*"\" \
	     		  -and -not -path \""*/tags/*"\" -print -delete; \
		done; \
	     fi; \
	done
	rm -rf build/ LICENSE test/try-reloading-dependency.asd test/hello-world-example asdf.lisp
	rm -rf test/hello-world-example.exe test/mkcl_*.dll # needed only on MS-Windows
	rm -rf .pc/ build-stamp debian/patches/ debian/debhelper.log debian/cl-asdf/ # debian crap
	${MAKE} -C doc clean

mrproper:
	git clean -xfd

test-upgrade: build/asdf.lisp
	./test/run-tests.sh -u ${l}
u: test-upgrade

test-clean-load: build/asdf.lisp
	./test/run-tests.sh -c ${l}

# test-glob has been replaced by t, and lisp by l, easier to type
test-lisp: build/asdf.lisp
	@cd test; ./run-tests.sh ${l} ${t}
t: test-lisp

test: test-lisp test-clean-load test-load-systems doc

test-load-systems: build/asdf.lisp
	./test/run-tests.sh -l ${l} ${s}

test-all-lisps: test-load-systems test-all-clean-load test-all-lisp test-all-upgrade

test-all-clean-load:
	@for lisp in ${lisps} ; do ${MAKE} test-clean-load l=$$lisp || exit 1 ; done

test-all-lisp:
	@for lisp in ${lisps} ; do ${MAKE} test-lisp l=$$lisp || exit 1 ; done

test-all-upgrade:
	@for lisp in ${ulisps} ; do ${MAKE} test-upgrade l=$$lisp || exit 1 ; done

test-all-no-upgrade: doc test-load-systems test-all-clean-load test-all-lisp

test-all: test-all-no-upgrade test-all-upgrade

test-all-lisp-no-stop:
	@for lisp in ${lisps} ; do ${MAKE} test-lisp l=$$lisp ; done ; :

test-all-upgrade-no-stop:
	@for lisp in ${ulisps} ; do ${MAKE} test-upgrade l=$$lisp ; done ; :

test-all-no-upgrade-no-stop: doc test-load-systems test-all-clean-load test-all-lisp-no-stop
	make --quiet check-all-test-results

test-all-no-stop: doc test-load-systems test-all-clean-load test-all-lisp-no-stop test-all-upgrade-no-stop
	make --quiet check-all-results

check-all-test-results:
	@A="`grep -L '[5-9][0-9] passing and 0 failing' build/results/*-test.text`" ; \
	if [ -n "$$A" ] ; then \
		echo "Unexpected test failures on these implementations:" ; \
		echo "$$A" ; \
		exit 1 ; \
	fi

check-all-upgrade-results:
	@A="`grep -L 'Upgrade test succeeded for ' build/results/*-upgrade.text`" ; \
	if [ -n "$$A" ] ; then \
		echo "Unexpected upgrade failures on these implementations:" ; \
		echo "$$A" ; \
		exit 1 ; \
	fi

check-all-results:
	@r=0 ; \
	make --quiet check-all-test-results || r=1 ; \
	make --quiet check-all-upgrade-results || r=1 ; \
	exit $r

extract: extract-all-tagged-asdf
extract-all-tagged-asdf: build/asdf.lisp
	./test/run-tests.sh -H

# Note that the debian git at git://git.debian.org/git/pkg-common-lisp/cl-asdf.git is stale,
# as we currently build directly from upstream at git://common-lisp.net/projects/asdf/asdf.git
debian-package:
	./bin/asdf-builder debian-package release

debian-package-from-master:
	./bin/asdf-builder debian-package master


# Replace SBCL's ASDF with the current one. -- NOT recommended now that SBCL has ASDF2.
# for casual users, just use (asdf:load-system :asdf)
replace-sbcl-asdf: build/asdf.lisp
	${SBCL} --eval '(compile-file "$<" :output-file (format nil "~Aasdf/asdf.fasl" (sb-int:sbcl-homedir-pathname)))' --eval '(quit)'

# Replace CCL's ASDF with the current one. -- NOT recommended now that CCL has ASDF2.
# for casual users, just use (asdf:load-system :asdf)
replace-ccl-asdf: build/asdf.lisp
	${CCL} --eval '(progn(compile-file "$<" :output-file (compile-file-pathname (format nil "~Atools/asdf.lisp" (ccl::ccl-directory))))(quit))'

WRONGFUL_TAGS := 1.37 1.1720 README RELEASE STABLE emp # It's not 1.37, it's 1.85! 1.37 is for the README.
# Delete wrongful tags from local repository
fix-local-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git tag -d $$i ; done

# Delete wrongful tags from remote repository
fix-remote-git-tags:
	for i in ${WRONGFUL_TAGS} ; do git push $${REMOTE:-cl.net} :refs/tags/$$i ; done

release-push:
	git checkout master
	git merge release
	git checkout release
	git merge master
	git checkout master

TODO:
	exit 2

release: TODO test-all test-on-other-machines-too debian-changelog debian-package send-mail-to-mailing-lists

.PHONY: install archive push doc website clean mrproper \
	test-forward-references test test-lisp test-upgrade test-forward-references \
	test-all test-all-lisps test-all-no-upgrade \
	debian-package release \
	replace-sbcl-asdf replace-ccl-asdf \
	fix-local-git-tags fix-remote-git-tags wc wc-driver wc-asdf

# RELEASE or PUSH checklist:
# make test-all
# make test-load-systems s=fare-all
# make bump v=3.0
# edit debian/changelog # RELEASE only...
# git commit
# git tag 3.0 # for example ...
# make debian-package
# git push
# git push origin 3.0 # for example...
# everything from here for RELEASE only
# make release-push archive website debian-package
# dput mentors ../*.changes
# send debian mentors request
# send announcement to asdf-announce, asdf-devel, etc.
# Move all fixed bugs from Fix Committed -> Fix Released on launchpad
#
## Users don't release as above, only maintainers do.
## Users, all you need to do is: make
## Vendors, you may want to test your implementation with: make test l=sbcl
