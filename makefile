# -*- mode: GNUmakefile; indent-tabs-mode: t -*-
# Cleaned up by Shinmera October 13, 2015

MAKE	?= make
PREFIX	?= /opt/clasp

all:
	./waf build_dboehm

configure:
	./waf update_dependencies
	./waf configure --prefix=$(PREFIX)

clean:
	./waf distclean

pull-sicl-master:
	(cd src/lisp/kernel/contrib/sicl; git pull origin master)
	$(MAKE) setup-cleavir

dump-local-config:
	cat $(CLASP_HOME)/wscript.config

print-config:
	$(info >> Makefile Configuration:)
	$(call varprint, CLASP_HOME)
	$(call varprint, EXTERNALS_CLASP_DIR)
	$(call varprint, LLVM_CONFIG)
	$(call varprint, TARGET_OS)
	$(call varprint, ADDRESS-MODEL)
	$(call varprint, PJOBS)
	$(call varprint, LLVM_CONFIG_DEBUG)
	$(call varprint, LLVM_CONFIG_RELEASE)
	$(call varprint, LLVM_BIN_DIR)
	$(call varprint, CLASP_CLANG_PATH)
	$(call varprint, CLASP_CLANGXX_PATH)
	$(call varprint, GIT_COMMIT)
	$(call varprint, CLASP_VERSION)
	$(call varprint, LIBATOMIC_OPS_SOURCE_DIR)
	$(call varprint, CLASP_APP_EXECS)
	$(call varprint, CLASP_APP_RESOURCES_DIR)
	$(call varprint, CLASP_APP_RESOURCES_LIB_COMMON_DIR)
	$(call varprint, CLASP_DEBUG_LLVM_LIB_DIR)
	$(call varprint, CLASP_RELEASE_LLVM_LIB_DIR)
	$(call varprint, CLASP_DEBUG_CXXFLAGS)
	$(call varprint, CLASP_DEBUG_LINKFLAGS)
	$(call varprint, CLASP_RELEASE_CXXFLAGS)
	$(call varprint, CLASP_RELEASE_LINKFLAGS)
	$(call varprint, DEVEMACS)
	$(call varprint, PYTHON2)
	$(call varprint, EXECUTABLE_DIR)
	$(call varprint, BINDIR)
	$(call varprint, EXECS)
	$(call varprint, PATH)
	$(call varprint, USE_CXXFLAGS)

clang-format:
	git ls-files src/ include/ \
	| perl -ne 'chomp;print "$$_\n" if -f $$_ and (/\.[hc][hcp]?p?$$/) and !-l and !m#^include/.+/generated#;' \
	| xargs -P$(PJOBS) -n1 --verbose clang-format -i


docker:
	time sudo docker-compose run clasp-build
	time sudo docker-compose build clasp
	time sudo docker-compose build cando-compile
	time sudo docker-compose run cando --help 

push-to-master:
	git fetch origin dev:testing
	git push origin testing
	git fetch origin testing:preview
	git push origin preview
	git fetch origin preview:master
	git push origin master

push-to-preview:
	git fetch origin dev:testing
	git push origin testing
	git fetch origin testing:preview
	git push origin preview

push-to-testing:
	git fetch origin dev:testing
	git push origin testing

analyze:
	./waf build_cboehmdc
	$(MAKE) analyze_rest

analyze_rest:
	./waf build_impsprep
	./waf analyze_clasp

push-cando-to-testing:
	git push origin dev
	git fetch origin dev:testing
	git push origin testing
	(cd extensions/cando; git push origin dev)
	(cd extensions/cando; git fetch origin dev:testing)
	(cd extensions/cando; git push origin testing)

cando-deploy:
	aws s3 cp s3://clasp-cando/docker-cando/cando-build.tgz cando-build.tgz
	aws s3 cp s3://clasp-cando/demos/demos.tar demos.tar
	docker-compose build cando-deploy
