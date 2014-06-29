ifeq ($(DEVELOPMENT_ENVIRONMENT),linuxClang)
CLASP_CXXFLAGS="-std=c++11"
else
CLASP_CXXFLAGS="-std=c++11 -stdlib=libc++"
endif

export REQUIRES_RTTI=$(CLASP_REQUIRES_RTTI)
BOEHM_VERSION=7.2
DMALLOC_VERSION=5.5.2
CC = $(CLASP_CC)
#export CXX = $(CLASP_CXX)
#export CXXFLAGS=$(CLASP_CXXFLAGS)
#export CXXFLAGS=$(CLASP_CXXFLAGS) -fvisibility=hidden
#export LDFLAGS=$(CLASP_LDFLAGS)

ifneq ($(CXXFLAGS),)
	BOOST_CXXFLAGS="cxxflags=$(CXXFLAGS)"
endif
ifneq ($(LDFLAGS),)
	BOOST_LDFLAGS="linkflags=$(LDFLAGS) -lc++"
endif


CLANG_SOURCE_DIR = clang
OPENMPI_SOURCE_DIR = openmpi-1.6.5
MPS_SOURCE_DIR = mps-temporary
ECL_SOURCE_DIR = ecl



all-dependencies:
	make subClean
	make setup
	make subAll

export LLVM_REVISION=207120

# export LLVM_SOURCE_DIR = llvm-$(LLVM_REVISION)
export LLVM_SOURCE_DIR = llvm


BOEHM_SOURCE_DIR = boehm-7.2



EXPAT_SOURCE_DIR = expat-2.0.1
ZLIB_SOURCE_DIR = zlib-1.2.8
GMP_SOURCE_DIR = gmp-5.0.5
BOOST_SOURCE_DIR = boost
#export BOOST_BUILD_SOURCE_DIR = boost-build-system
BOOST_BUILD_SOURCE_DIR = $(BOOST_SOURCE_DIR)/tools/build/v2
LLDB_SOURCE_DIR = lldb

printenv:
	echo EXTERNALS_DIR=$(EXTERNALS_DIR)
	echo CLASP_APP_INSTALL_ROOT=$(CLASP_APP_INSTALL_ROOT)
	printenv




setup:
	install -d $(CLASP_APP_RESOURCES_DIR)
	install -d $(CLASP_APP_RESOURCES_EXTERNALS_DIR)
	install -d $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_DIR)
	install -d $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR)
	make subClean
	make boostbuild2-build  #
	make boehm-setup        #
	make llvm-setup		#
	make openmpi-setup
	make boost-setup
	make ecl-setup
	make gmp-setup
	make expat-setup
	make zlib-setup
	make readline-setup
#	make clang-setup This is redundant, it gets configured with llvm
#	make lldb-setup

subAll sa:
	make boehm-build
	make llvm-build
	make openmpi-build
	make boost-build
	make ecl-build
	make gmp-build
	make expat-build
	make zlib-build
	make readline-build
	make subBundle
#	-make ez_setup
#	-make pyOpenGl-cleanBuild


subClean:
	make openmpi-clean
	make ecl-clean
	make dmalloc-clean
	make boehm-clean
	make readline-clean
	make expat-clean
	make zlib-clean
	make boost-clean
#	make clang-clean Redundant.
	make llvm-clean



rpath-fix:
	make boost-rpath-fix
	make llvm-rpath-fix
	make gmp-rpath-fix
	make zlib-rpath-fix
	make readline-rpath-fix
	make expat-rpath-fix


openmpi-clean:
	-(cd $(OPENMPI_SOURCE_DIR); make clean)

openmpi-setup:
	-(cd $(OPENMPI_SOURCE_DIR); ./configure --prefix=$(CLASP_APP_RESOURCES_EXTERNALS_COMMON_DIR))

#
# There is a problem in openmpi where they have "(default: "DEFAULT_OUTFILE")    "
# it is tripping the warning -Wreserved-user-defined-literal
openmpi-build:
	-(cd $(OPENMPI_SOURCE_DIR); CXXFLAGS="-Wno-reserved-user-defined-literal"; make all install)

mps-setup:
	-(cd $(MPS_SOURCE_DIR); ./configure --prefix=$(CLASP_APP_RESOURCES_EXTERNALS_COMMON_DIR))


mps-build:
	-(cd $(MPS_SOURCE_DIR); make -j$(COMPILE_PROCESSORS) install)


lldb-get:
	svn co http://llvm.org/svn/llvm-project/lldb/trunk lldb

lldb-setup:
	-mkdir -p $(LLVM_SOURCE_DIR)/tools/lldb/build
	(cd $(LLVM_SOURCE_DIR)/tools/lldb/build; \
		CC=clang; CXX=clang++; CXXFLAGS="-I$(HOME)/Development/cando/externals/src/libcxx/include -std=c++11 -stdlib=libc++"\
		../../../configure \
		--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR);)

lldb-build:
	(cd $(LLDB_SOURCE_DIR); xcodebuild -jobs 4 -project lldb.xcodeproj)


llvm-rpath-fix:
	make llvm-rpath-fix-debug
	make llvm-rpath-fix-release


ecl-setup:
	make ecl-clean
	(cd $(ECL_SOURCE_DIR); ./configure --prefix=$(CLASP_APP_RESOURCES_EXTERNALS_COMMON_DIR))

ecl-build:
	(cd $(ECL_SOURCE_DIR); make; make install)  2>&1 | tee _ecl.log

ecl-clean:
	-(rm -rf $(ECL_SOURCE_DIR)/build)





llvm-clean:
	make llvm-clean-release
	make llvm-clean-debug

llvm-clean-release:
	-(rm -rf $(LLVM_SOURCE_DIR)/build-release;)

llvm-clean-debug:
	-(rm -rf $(LLVM_SOURCE_DIR)/build-debug;)


llvm-setup-rtti-test:
	echo REQUIRES_RTTI=$(REQUIRES_RTTI)

llvm-setup:
	echo REQUIRES_RTTI=$(REQUIRES_RTTI)
	-ln -s clang llvm/tools/clang
	-ln -s libcxx llvm/projects
	make llvm-setup-debug
	make llvm-setup-release



llvm-build:
	make llvm-debug
	make llvm-release

# build llvm with VERBOSE=1 to see commands as they execute  - also set -j1 so multiple builds output aren't interleaved
#	(cd $(LLVM_SOURCE_DIR)/build-debug; export VERBOSE=1;  make VERBOSE=1 -j$(COMPILE_PROCESSORS) ; make install) 2>&1 | tee _llvm-debug.log

llvm-debug:
	(cd $(LLVM_SOURCE_DIR)/build-debug; make -j$(COMPILE_PROCESSORS) ; make install) 2>&1 | tee _llvm-debug.log

llvm-release:
	(cd $(LLVM_SOURCE_DIR)/build-release; make -j$(COMPILE_PROCESSORS) ; make install) 2>&1 | tee _llvm-release.log




boost-build:
	make boost-build-debug
	make boost-build-release


boost-build-debug:
	echo LDFLAGS=$(LDFLAGS)    processed = $(BOOST_LDFLAGS)
	(cd $(BOOST_SOURCE_DIR); \
				bjam toolset=$(BOOST_TOOLSET)  \
				$(BOOST_CXXFLAGS) $(BOOST_LDFLAGS) \
				--with-filesystem --with-date_time	\
				--with-serialization --with-iostreams	\
				--with-program_options --with-regex	\
				--with-system --with-mpi\
				include=../$(ZLIB_SOURCE_DIR) linkflags=-L../$(ZLIB_SOURCE_DIR)\
				--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_DIR) \
				debug link=static\
				-j$(COMPILE_PROCESSORS) \
				install ) | tee _boost.log 


boost-build-release:
	(cd $(BOOST_SOURCE_DIR); bjam toolset=$(BOOST_TOOLSET)   \
				$(BOOST_CXXFLAGS) $(BOOST_LDFLAGS) \
				--with-filesystem --with-date_time	\
				--with-serialization --with-iostreams	\
				--with-program_options --with-regex	\
				--with-system --with-mpi \
				include=../$(ZLIB_SOURCE_DIR) linkflags=-L../$(ZLIB_SOURCE_DIR)\
				--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR) \
				release \
				-j$(COMPILE_PROCESSORS) \
				install ) | tee _boost.log 

#				include=$(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR) \


llvm-doxygen:
	-mkdir -p $(LLVM_SOURCE_DIR)/build-doxygen
	(cd $(LLVM_SOURCE_DIR)/build-doxygen; export REQUIRES_RTTI=1; \
		../configure --enable-doxygen \
		--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_DIR);)
	(cd $(LLVM_SOURCE_DIR)/build-dbg; export REQUIRES_RTTI=1; make -j$(COMPILE_PROCESSORS) REQUIRES_RTTI=1; make install) 2>&1 | tee _llvm-doxygen.log




clang-setup:
	make clang-setup-release


clang:
	make clang-release


clang-clean:
	make clang-clean-release

clang-clean-release:
	-(rm -rf $(LLVM_SOURCE_DIR)/clang-build;)


clang-release:
	(cd $(LLVM_SOURCE_DIR)/clang-build; export REQUIRES_RTTI=1; make -j$(COMPILE_PROCESSORS) REQUIRES_RTTI=1 ; make install) 2>&1 | tee _clang.log



clang-setup-release:
	-mkdir -p $(LLVM_SOURCE_DIR)/clang-build
	(cd $(LLVM_SOURCE_DIR)/clang-build; export REQUIRES_RTTI=1; \
		../configure --enable-optimized --enable-shared \
		--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR);)


gmp-clean:
	(cd $(GMP_SOURCE_DIR); make clean ; rm -f gen-fib)

gmp-setup:
	(cd $(GMP_SOURCE_DIR); \
		CXXFLAGS=$(CLASP_CXXFLAGS); \
		./configure --prefix=$(CLASP_APP_RESOURCES_EXTERNALS_COMMON_DIR) --enable-cxx)
	echo "If this failed - use make gmp-clean first to clear out old configuration"
	echo "gmpXXX/gen-fib sticks around even after gmp-clean - maybe delete it???"


gmp-build-n:
	(cd $(GMP_SOURCE_DIR); make install -n) | tee _gmp.log





boost-setup:
#	cp project-config.jam $(BOOST_SOURCE_DIR)/project-config.jam
#	cp user-config.jam $(BOOST_SOURCE_DIR)/tools/build/v2/user-config.jam
	echo do nothing

boost-build-a-n:
	(cd $(BOOST_SOURCE_DIR); bjam toolset=$(BOOST_TOOLSET) -a -n  \
				cxxflags="$(CXXFLAGS)" linkflags="$(LDFLAGS)" \
				--with-filesystem --with-date_time	\
				--with-serialization --with-iostreams	\
				--with-program_options --with-regex	\
				--with-python --with-system  --with-thread \
				--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_DIR) \
				debug  \
				-j$(COMPILE_PROCESSORS) \
				install ) | tee _boost.log 


boost-clean:
	-(cd $(BOOST_SOURCE_DIR); bjam --toolset=$(BOOST_TOOLSET) --clean debug)
	-(cd $(BOOST_SOURCE_DIR); bjam --toolset=$(BOOST_TOOLSET) --clean release)



expat-setup:
	(cd $(EXPAT_SOURCE_DIR); \
		./configure --prefix=$(CLASP_APP_RESOURCES_EXTERNALS_COMMON_DIR); )

expat-build:
	(cd $(EXPAT_SOURCE_DIR); \
			make -j1 install) | tee _expat.log


expat-clean:
	-(cd $(EXPAT_SOURCE_DIR); make clean )




zlib-setup:
	(cd $(ZLIB_SOURCE_DIR); \
		./configure -shared --prefix=$(CLASP_APP_RESOURCES_EXTERNALS_COMMON_DIR);)

zlib-build:
	(cd $(ZLIB_SOURCE_DIR); \
			make -j1 install) | tee _zlib.log


zlib-clean:
	-(cd $(ZLIB_SOURCE_DIR); make clean )





dmalloc-setup:
	(cd dmalloc-$(DMALLOC_VERSION); \
		./configure --enable-cxx --prefix=$(CLASP_APP_RESOURCES_EXTERNALS_COMMON_DIR);)
dmalloc-build:
	make dmalloc-compile
	make dmalloc-install

dmalloc-compile:
	(cd dmalloc-$(DMALLOC_VERSION); make -j1 cxx light | tee _dmalloc.log)

dmalloc-install:
	(cd dmalloc-$(DMALLOC_VERSION); make -j1 light installcxx install | tee _dmalloc_install.log)



dmalloc-clean:
	-(cd dmalloc-$(DMALLOC_VERSION); make clean )






boehm-setup:
	(cd $(BOEHM_SOURCE_DIR); \
		export ALL_INTERIOR_PTRS=1; \
		CFLAGS="-DUSE_MMAP" \
		./configure --enable-handle-fork --enable-cplusplus --prefix=$(CLASP_APP_RESOURCES_EXTERNALS_COMMON_DIR);)
boehm-build:
	make boehm-compile
	make boehm-install

boehm-compile:
	(cd $(BOEHM_SOURCE_DIR); make -j1 | tee _boehm.log)

boehm-install:
	(cd $(BOEHM_SOURCE_DIR); make -j1 install | tee _boehm_install.log)



boehm-clean:
	-(cd $(BOEHM_SOURCE_DIR); make clean )









readline-build:
	make readline-compile
	make readline-install

readline-setup:
	(cd readline-$(READLINE_VERSION); \
		./configure --enable-shared --prefix=$(CLASP_APP_RESOURCES_EXTERNALS_COMMON_DIR);)
	sed -e 's/-dynamic/-dynamiclib/g' <readline-$(READLINE_VERSION)/shlib/Makefile  >readline-$(READLINE_VERSION)/shlib/Makefile.good
	mv readline-$(READLINE_VERSION)/shlib/Makefile.good readline-$(READLINE_VERSION)/shlib/Makefile


readline-compile:
	(cd readline-$(READLINE_VERSION); make -j1 | tee _readline.log)

readline-install:
	(cd readline-$(READLINE_VERSION); make -j1 install | tee _readline_install.log)



readline-clean:
	-(cd readline-$(READLINE_VERSION); make clean )



cmake-setup:
	(cd $(CMAKE_VERSION); ./configure;)

cmake-install:
	(cd $(CMAKE_VERSION); make install; )


openmm-setup:
	install -d $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR)/openmm/build
	(cd $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR)/openmm/build; \
		ccmake -DCMAKE_INSTALL_PREFIX=$(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR)/openmm \
			-DOPENMM_BUILD_AMOEBA_CUDA_LIB=OFF \
			-DOPENMM_BUILD_CUDA_LIB=OFF \
			-DCMAKE_OSX_DEPLOYMENT_TARGET=10.7 \
			-DCMAKE_OSX_ARCHITECTURES=x86_64 \
			-DOPENMM_BUILD_FREE_ENERGY_CUDA_LIB=OFF \
			-i $(CLASP_EXTERNALS_SRC)/$(OPENMM_VERSION);)

xpath-install:
	(cd py-dom-xpath-0.1; python setup.py install)



openmm-install:
	(cd $(CLASP_P_RESOURCES_EXTERNALS_RELEASE_DIR)/openmm/build; make install )


pyopenmm-install:
	(cd PyOpenMM-3.0.0;python setup.py build; python setup.py install)





subBundle sb:
	install -d $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR)/bin
	install -c jamfile.jam.lib $(CLASP_APP_RESOURCES_EXTERNALS_DIR)/jamfile.jam
	-install -c $(OPENMM_INSTALL)/lib/lib* $(CLASP_APP_LIB_DIR)
	-install -c $(OPENMM_INSTALL)/lib/plugins/* $(CLASP_APP_LIB_DIR)/plugins
	make rpath-fix
# This first link allows LibTooling to find the clang include directories relative to the clasp executable path
	install -d $(CLASP_APP_BIN_DIR)
	-ln -s $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR)/lib $(CLASP_APP_BIN_DIR)/../lib
	echo IF YOU GOT HERE EVERYTHING IS GOING TO BE JUST FINE!!!




############################################################
#
# linuxClang linuxClang linuxClang linuxClang linuxClang linuxClang linuxClang linuxClang linuxClang
#
##

ifeq ($(DEVELOPMENT_ENVIRONMENT),linuxClang)
#
# Set clang-setup --prefix to $(CLASP_APP_RESOURCES_DIR)
#
# This will also move the clang executable into the CLASP_APP_RESOURCES_DIR
# so adjust your path accordingly
#




#linuxClang
llvm-setup-debug:
	-mkdir -p $(LLVM_SOURCE_DIR)/build-debug
	(cd $(LLVM_SOURCE_DIR)/build-debug; \
		../configure --enable-targets=x86_64 --enable-debug-symbols --enable-debug-runtime \
		--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_DIR) --with-gcc-toolchain=$(HOME)/local/gcc-4.8.3 \
		CC=$(HOME)/local/gcc-4.8.3/bin/gcc \
		CXX=$(HOME)/local/gcc-4.8.3/bin/g++ \
		CXXFLAGS="-static-libstdc++ -static-libgcc" \
		CFLAGS="-static-libgcc" \
		--enable-shared=no --enable-cxx11 )


#linuxClang
llvm-setup-release:
	-mkdir -p $(LLVM_SOURCE_DIR)/build-release
	(cd $(LLVM_SOURCE_DIR)/build-release; \
		../configure --enable-targets=x86_64  --enable-optimized --enable-assertions  \
		--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR) --with-gcc-toolchain=$(HOME)/local/gcc-4.8.3 \
		CC=$(HOME)/local/gcc-4.8.3/bin/gcc \
		CXX=$(HOME)/local/gcc-4.8.3/bin/g++ \
		CXXFLAGS="-static-libstdc++ -static-libgcc" \
		CFLAGS="-static-libgcc" \
		--enable-shared=no --enable-cxx11 )




boostbuild2-build:
	(cd $(BOOST_BUILD_SOURCE_DIR); ./bootstrap.sh; ./b2 toolset=$(CLASP_TOOLSET) install --prefix=$(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR))


gmp-build:
	(cd $(GMP_SOURCE_DIR); make install) | tee _gmp.log







llvm-rpath-fix-debug:
	echo Do nothing


llvm-rpath-fix-release:
	echo Do nothing

boost-rpath-fix:
	echo Do nothing

readline-rpath-fix:
	echo Do nothing

zlib-rpath-fix:
	echo Do nothing

expat-rpath-fix:
	echo Do nothing

gmp-rpath-fix:
	echo Do nothing

endif


############################################################
#
# darwin-CLANG darwin-CLANG darwin-CLANG darwin-CLANG darwin-CLANG darwin-CLANG darwin-CLANG darwin-CLANG darwin-CLANG
#
##

ifeq ($(DEVELOPMENT_ENVIRONMENT),darwinClang)

export RPATH_RELEASE_FIX = @executable_path/../Resources/externals/release/lib
export RPATH_DEBUG_FIX = @executable_path/../Resources/externals/debug/lib
export RPATH_COMMON_FIX = @executable_path/../Resources/externals/common/lib


#darwinClang
llvm-setup-debug:
	-mkdir -p $(LLVM_SOURCE_DIR)/build-debug
	(cd $(LLVM_SOURCE_DIR)/build-debug; \
		../configure --enable-targets=x86_64 \
			--enable-debug-symbols --enable-debug-runtime \
			--enable-cxx11 \
			--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_DIR);)

#darwinClang
llvm-setup-release:
	-mkdir -p $(LLVM_SOURCE_DIR)/build-release
	(cd $(LLVM_SOURCE_DIR)/build-release; \
		../configure --with-clang-srcdir=../$(CLANG_SOURCE_DIR)  --enable-targets=x86_64  --enable-optimized --enable-assertions  \
			--enable-cxx11 \
			--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR);)

gmp-build:
	(cd $(GMP_SOURCE_DIR); make install) | tee _gmp.log
	(cd $(GMP_SOURCE_DIR); source fixlibgmpxx.sh)   # fixes libgmpxx so that it uses libc++ on OS X


boostbuild2-build:
	(cd $(BOOST_BUILD_SOURCE_DIR); ./bootstrap.sh; ./b2 toolset=clang-darwin install --prefix=$(CLASP_APP_RESOURCES_EXTERNALS_COMMON_DIR))




# boost-build-debug:
# 	(cd $(BOOST_SOURCE_DIR); bjam toolset=$(BOOST_TOOLSET)   \
# 				cxxflags="-std=c++11 -stdlib=libc++" linkflags="-std=c++11 -stdlib=libc++" \
# 				--with-filesystem --with-date_time	\
# 				--with-serialization --with-iostreams	\
# 				--with-program_options --with-regex	\
# 				--with-python --with-system  --with-thread \
# 				--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_DIR) \
# 				debug \
# 				-j$(COMPILE_PROCESSORS) \
# 				install ) | tee _boost.log 
# boost-build-release:
# 	(cd $(BOOST_SOURCE_DIR); bjam toolset=$(BOOST_TOOLSET)   \
# 				cxxflags="-std=c++11 -stdlib=libc++" linkflags="-std=c++11 -stdlib=libc++" \
# 				--with-filesystem --with-date_time	\
# 				--with-serialization --with-iostreams	\
# 				--with-program_options --with-regex	\
# 				--with-python --with-system  --with-thread \
# 				--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_DIR) \
# 				release \
# 				-j$(COMPILE_PROCESSORS) \
# 				install ) | tee _boost.log 

# boost-build-a-n:
# 	(cd $(BOOST_SOURCE_DIR); bjam toolset=$(BOOST_TOOLSET) -a -n  \
# 				cxxflags="-std=c++11 -stdlib=libc++" linkflags="-std=c++11 -stdlib=libc++" \
# 				--with-filesystem --with-date_time	\
# 				--with-serialization --with-iostreams	\
# 				--with-program_options --with-regex	\
# 				--with-python --with-system  --with-thread \
# 				--prefix=$(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_DIR) \
# 				debug  \
# 				-j$(COMPILE_PROCESSORS) \
# 				install ) | tee _boost.log 

gmp-rpath-fix:
	install_name_tool -id $(RPATH_COMMON_FIX)/libgmp.dylib $(CLASP_APP_RESOURCES_EXTERNALS_COMMON_LIB_DIR)/libgmp.dylib
	install_name_tool -id $(RPATH_COMMON_FIX)/libgmpxx.dylib $(CLASP_APP_RESOURCES_EXTERNALS_COMMON_LIB_DIR)/libgmpxx.dylib

zlib-rpath-fix:
	install_name_tool -id $(RPATH_COMMON_FIX)/libz.1.dylib $(CLASP_APP_RESOURCES_EXTERNALS_COMMON_LIB_DIR)/libz.1.dylib

#
# On OS X you need to change the write privilages of the readline library - go figure
#
readline-rpath-fix:
	chmod u+w $(CLASP_APP_RESOURCES_EXTERNALS_COMMON_LIB_DIR)/libreadline.$(READLINE_VERSION).dylib
	install_name_tool -id $(RPATH_COMMON_FIX)/libreadline.$(READLINE_VERSION).dylib $(CLASP_APP_RESOURCES_EXTERNALS_COMMON_LIB_DIR)/libreadline.$(READLINE_VERSION).dylib

expat-rpath-fix:
	install_name_tool -id $(RPATH_COMMON_FIX)/libexpat.1.dylib $(CLASP_APP_RESOURCES_EXTERNALS_COMMON_LIB_DIR)/libexpat.1.dylib



llvm-rpath-fix-release:
#	-install_name_tool -id $(RPATH_RELEASE_FIX)/libLLVM-$(LLVM_VERSION).dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libLLVM-$(LLVM_VERSION).dylib
#	-install_name_tool -id $(RPATH_RELEASE_FIX)/libclang.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libclang.dylib


llvm-rpath-fix-debug:
#	-install_name_tool -id $(RPATH_DEBUG_FIX)/libLLVM-$(LLVM_VERSION).dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libLLVM-$(LLVM_VERSION).dylib
#	-install_name_tool -id $(RPATH_DEBUG_FIX)/libclang.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libclang.dylib




boost-rpath-fix:
	echo The following sets up the libraries so that clasp can find them and so that they can find each other
	echo Fixing rpaths for release boost libraries
	install_name_tool -id $(RPATH_RELEASE_FIX)/libboost_program_options.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_program_options.dylib
	install_name_tool -id $(RPATH_RELEASE_FIX)/libboost_iostreams.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_iostreams.dylib
	install_name_tool -id $(RPATH_RELEASE_FIX)/libboost_regex.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_regex.dylib
	install_name_tool -id $(RPATH_RELEASE_FIX)/libboost_filesystem.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_filesystem.dylib
	install_name_tool -id $(RPATH_RELEASE_FIX)/libboost_date_time.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_date_time.dylib
	-install_name_tool -id $(RPATH_RELEASE_FIX)/libboost_mpi.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_mpi.dylib
	-install_name_tool -change libboost_serialization.dylib $(RPATH_RELEASE_FIX)/libboost_serialization.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_mpi.dylib 
#	-install_name_tool -id $(RPATH_RELEASE_FIX)/libboost_thread.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_thread.dylib
	install_name_tool -id $(RPATH_RELEASE_FIX)/libboost_serialization.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_serialization.dylib
#	install_name_tool -id $(RPATH_RELEASE_FIX)/libboost_python.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_python.dylib
	install_name_tool -id $(RPATH_RELEASE_FIX)/libboost_system.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_system.dylib
#	install_name_tool -change libboost_system.dylib $(RPATH_RELEASE_FIX)/libboost_system.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_thread.dylib
	install_name_tool -change libboost_system.dylib $(RPATH_RELEASE_FIX)/libboost_system.dylib $(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)/libboost_filesystem.dylib
	echo Fixing rpaths for debug boost libraries
	install_name_tool -id $(RPATH_DEBUG_FIX)/libboost_program_options.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_program_options.dylib
	install_name_tool -id $(RPATH_DEBUG_FIX)/libboost_iostreams.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_iostreams.dylib
	install_name_tool -id $(RPATH_DEBUG_FIX)/libboost_regex.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_regex.dylib
	install_name_tool -id $(RPATH_DEBUG_FIX)/libboost_filesystem.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_filesystem.dylib
	install_name_tool -id $(RPATH_DEBUG_FIX)/libboost_date_time.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_date_time.dylib
	-install_name_tool -id $(RPATH_DEBUG_FIX)/libboost_mpi.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_mpi.dylib
	-install_name_tool -change libboost_serialization.dylib $(RPATH_DEBUG_FIX)/libboost_serialization.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_mpi.dylib 
#	-install_name_tool -id $(RPATH_DEBUG_FIX)/libboost_thread.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_thread.dylib
	install_name_tool -id $(RPATH_DEBUG_FIX)/libboost_serialization.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_serialization.dylib
#	install_name_tool -id $(RPATH_DEBUG_FIX)/libboost_python.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_python.dylib
	install_name_tool -id $(RPATH_DEBUG_FIX)/libboost_system.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_system.dylib
#	install_name_tool -change libboost_system.dylib $(RPATH_DEBUG_FIX)/libboost_system.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_thread.dylib
	install_name_tool -change libboost_system.dylib $(RPATH_DEBUG_FIX)/libboost_system.dylib $(CLASP_APP_RESOURCES_EXTERNALS_DEBUG_LIB_DIR)/libboost_filesystem.dylib



endif








############################################################
#
# kraken kraken kraken kraken kraken kraken kraken kraken kraken
#
##

ifeq ($(DEVELOPMENT_ENVIRONMENT),kraken)
subAll sa:
	make boost-jam
	make boost
	make expat
	make readline
	make subBundle
#	-make ez_setup
#	-make pyOpenGl-cleanBuild


subClean:
	make boost-jam-clean
	make boost-clean
	make expat-clean



boost-jam:
	(cd $(BJAM_SOURCE_DIR); sh ./build.sh $(BOOST_TOOLSET))
	install -d $(OS_BUILDTOP)/bin
	install -c $(BJAM) $(OS_BUILDTOP)/bin


boost-jam-clean:
	-(cd $(BJAM_SOURCE_DIR); rm -rf $(BJAM_EXE_LOCAL_DIR) bootstrap)



boost-fresh:
	(cd $(BOOST_SOURCE_DIR); \
		$(BJAM) -j$(COMPILE_PROCESSORS) -a \
			-d+2 \
			--toolset=$(BOOST_TOOLSET) \
			--prefix=$(OS_BUILDTOP) \
			--with-filesystem \
			--with-date_time\
			--with-serialization \
			--with-program_options \
			--with-mpi \
			--with-thread \
			--with-regex \
			--debug-configuration \
			link=static \
			install \
			2>&1 | tee _boost.log )
	make boost-rpath-fix

boost-build:
	(cd $(BOOST_SOURCE_DIR); \
		$(BJAM) -j$(COMPILE_PROCESSORS) \
			-d+2 \
			--toolset=$(BOOST_TOOLSET) \
			--prefix=$(OS_BUILDTOP) \
			--with-filesystem \
			--with-date_time\
			--with-serialization \
			--with-program_options \
			--with-mpi \
			--with-thread \
			--with-regex \
			--debug-configuration \
			link=static \
			install \
			2>&1 | tee _boost.log )
	make boost-rpath-fix

boost-n:
	(cd $(BOOST_SOURCE_DIR); \
		$(BJAM) -j$(COMPILE_PROCESSORS) \
			-n \
			-d+2 \
			--toolset=$(BOOST_TOOLSET) \
			--prefix=$(OS_BUILDTOP) \
			--with-filesystem \
			--with-date_time\
			--with-serialization \
			--with-program_options \
			--with-mpi \
			--with-regex \
			--with-thread \
			link=static \
			install )



boost-rpath-fix:
	echo Nothing to fix

boost-clean:
	-(cd $(BOOST_SOURCE_DIR); $(BJAM) --toolset=$(BOOST_TOOLSET) --cleanAll clean)



expat-build:
	(cd $(EXPAT_SOURCE_DIR); \
			make -j1 install) | tee _expat.log

expat-clean:
	-(cd $(EXPAT_SOURCE_DIR); make clean )


readline-clean:
	-(cd $(READLINE_SOURCE_DIR); make clean )

subBundle sb:
	install -d $(CLASP_APP_LIB_DIR)
	install -d $(OS_BUILDTOP)/lib
	install -c jamfile.linux $(OS_BUILDTOP)/lib/jamfile.jam
	install -c $(OS_BUILDTOP)/lib/* $(CLASP_APP_LIB_DIR)




#	install -c $(OS_BUILDTOP)/lib/libboost_filesystem-$(BOOST_VERSION).dylib $(CLASP_APP_LIB_DIR)
#	install -c $(OS_BUILDTOP)/lib/libboost_program_options-$(BOOST_VERSION).dylib $(CLASP_APP_LIB_DIR)
#	install -c $(OS_BUILDTOP)/lib/libboost_python-$(BOOST_VERSION).dylib $(CLASP_APP_LIB_DIR)
#	install -c /usr/local/lib/libwx* $(CLASP_APP_LIB_DIR)
#	install -c lib
#	install	-c /System/Library/Frameworks/Python.framework/Versions/

endif




#
# To fix python so that it compiles on os x without barfing when it tries to handle unicodedata module
# make entries in Makefile.pre and Makefile.pre.in for compileall.py ignore errors
#
#
unexport	PYTHONPATH
unexport	PYTHONHOME
python-rpath-fix:
	cp $(PYTHON_SOURCE_DIR)/configure.in $(PYTHON_SOURCE_DIR)/configure.in.orig
	sed 's/arch_only ppc/arch_only i386/g' $(PYTHON_SOURCE_DIR)/configure.in.orig >$(PYTHON_SOURCE_DIR)/configure.in


python:
	make python-clean
	make python-config
	make python-compile


python-config:
	(cd $(PYTHON_SOURCE_DIR); \
		./configure \
			--enable-framework \
			--enable-unicode=ucs2 \
			--enable-universalsdk \
			--prefix=$(EXTERNALS_DIR) 2>&1 | tee _python-config.log;\
	)



python-compile:
	make -j 1 -C $(PYTHON_SOURCE_DIR) frameworkinstall 2>&1 | tee _python-compile.log

python-clean:
	-make -C $(PYTHON_SOURCE_DIR) clean


#
# PyOpenGL
#
#


pyOpenGl-all:
	-make pyOpenGl-clean
	-make pyOpenGl-build
	-make pyOpenGl-install

pyOpenGl-cleanBuild:
	-make pyOpenGl-clean
	-make pyOpenGl-build

pyOpenGl-clean:
	-(cd $(PYOPENGL_SOURCE_DIR); python setup.py clean )

pyOpenGl-build:
	-(cd $(PYOPENGL_SOURCE_DIR); python setup.py build )

pyOpenGl-install:
	-(cd $(PYOPENGL_SOURCE_DIR); python setup.py install )

#
#
# wxPython
#
#
wxPython-all:
	-make wxPython-nuke
	-make wxPython-create
	-make wxPython-config
	echo GOING INTO wxPython-wxWidgets
	-make wxPython-wxWidgets
	echo GOING INTO wxPython-python
	-make wxPython-wxPython
	echo GOING INTO wxPython-install
	-make wxPython-install

wxPython-rest:
	echo GOING INTO wxPython-wxWidgets
	-make wxPython-wxWidgets
	echo GOING INTO wxPython-python
	-make wxPython-wxPython
	echo GOING INTO wxPython-install


wxPython-nuke:
	-rm -rf $(WXPYTHON_SOURCE_DIR)/macbuild

wxPython-create:
	-(cd $(WXPYTHON_SOURCE_DIR); mkdir -p macbuild)

wxPython-config:
	-(cd $(WXPYTHON_SOURCE_DIR)/macbuild; \
		../configure --prefix $(EXTERNALS_DIR)\
			--enable-monolithic \
			--with-mac \
			--enable-unicode \
			--with-open-gl\
	)

wxPython-wxWidgets:
	-(cd $(WXPYTHON_SOURCE_DIR)/macbuild; \
		make -j $(COMPILE_PROCESSORS);\
	)

#		make -j $(COMPILE_PROCESSORS) -C contrib/src/gizmos\
#		make -j $(COMPILE_PROCESSORS) -C contrib/src/stc;\

#working
#WX_CONFIG=$(EXTERNALS_DIR)/bin/wx_config;


export WXWIN=$(WXPYTHON_SOURCE_DIR)
export WX_CONFIG=$(EXTERNALS_DIR)/bin/wx-config

#
#  Took out these options (--inplace --debug ) and now it compiles?!?!
#
wxPython-wxPython:
	( echo WX_CONFIG==== $(WX_CONFIG); \
		cd $(WXPYTHON_SOURCE_DIR)/wxPython;\
		python setup.py build_ext \
				WX_CONFIG=$(WX_CONFIG)\
				BUILD_STC=1\
				BUILD_GLCANVAS=1;\
	)


wxPython-install:
	-(cd $(WXPYTHON_SOURCE_DIR)/macbuild; make install)

wxPython-clean:
	make wxPython-nuke
	(cd $(WXPYTHON_SOURCE_DIR)/wxPython; python setup.py clean)


