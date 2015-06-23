testints.bc: testints.cc
	/usr/bin/clang++ -O3 testints.cc -o testints

testalign.bc: testalign.cc
	/usr/bin/clang++ -O3 testalign.cc -o testalign

testfixnum.bc: testfixnum.cc
	/usr/bin/clang++ -O0 testfixnum.cc -o testfixnum

testReturnMultipleValues.bc: testReturnMultipleValues.cc
	/usr/bin/clang++ -O0 -S -emit-llvm testReturnMultipleValues.cc

testReturnMultipleValues: testReturnMultipleValues.cc
	/usr/bin/clang++ -o testReturnMultipleValues testReturnMultipleValues.cc

computedGoto: computedGoto.cc
	/usr/bin/clang++ -o computedGoto computedGoto.cc

targv0: targv0.cc
	clang++ -o targv0 targv0.cc

testSeek: testSeek.cc
	clang++  -o testSeek testSeek.cc


wrapfun: wrapfun.cc
	clang++ -std=c++11 -stdlib=libc++ -o wrapfun wrapfun.cc


jmp: jmp.cc
	clang++ -o jmp jmp.cc


testarr: testarr.cc
	clang++ -Wgnu-array-member-paren-init -std=c++11 -stdlib=libc++ -O0 -g -o testarr testarr.cc

testarr2: testarr.cc
	g++ -std=c++11 -stdlib=libc++ -O0 -g -o testarr testarr.cc


test0array: test0array.cc
	clang++ -std=c++11 -stdlib=libc++ -O0 -g -o test0array test0array.cc

test0array_other: test0array.cc
	clang++ -v "-cc1" "-triple" "x86_64-apple-macosx10.7.0" "-fsyntax-only" "-disable-free" "-main-file-name" "testAST.cc" "-mrelocation-model" "pic" "-pic-level" "2" "-mdisable-fp-elim" "-masm-verbose" "-munwind-tables" "-target-cpu" "core2" "-target-linker-version" "236.4" "-v" "-resource-dir" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr//lib/clang/5.1" "-D" "DONT_USE_MPS" "-D" "EXPAT" "-D" "INCLUDED_FROM_CLASP" "-D" "INHERITED_FROM_SRC" "-D" "NDEBUG" "-D" "READLINE" "-D" "USE_GCVECTOR_MALLOC_ALLOCATOR" "-D" "_ADDRESS_MODEL_64" "-D" "_DEBUG" "-D" "_GNU_SOURCE" "-D" "_RELEASE_BUILD" "-D" "_TARGET_OS_DARWIN" "-D" "__STDC_CONSTANT_MACROS" "-D" "__STDC_FORMAT_MACROS" "-D" "__STDC_LIMIT_MACROS" "-D" "USE_MPS" "-I" "../../src" "-I" "../../src/asttooling" "-I" "../../src/asttooling/bin/clang-darwin-4.2.1/release/link-static/mps-off" "-I" "../../src/clbind/bin/clang-darwin-4.2.1/release/link-static/mps-off" "-I" "../../src/core/bin/clang-darwin-4.2.1/release/link-static/mps-off" "-I" "../../src/llvmo/bin/clang-darwin-4.2.1/release/link-static/mps-off" "-I" "/Library/Frameworks/EPD64.framework/Versions/7.2/include/python2.7" "-I" "/Users/meister/Development/cando/clasp/build/cando.app/Contents/Resources/externals/common/include" "-I" "/Users/meister/Development/cando/clasp/build/cando.app/Contents/Resources/externals/release/include" "-stdlib=libc++" "-O3" "-Wno-deprecated-register" "-std=c++11" "-fdeprecated-macro" "-fdebug-compilation-dir" "/Users/meister/Development/cando/clasp/src/asttooling" "-ferror-limit" "19" "-fmessage-length" "229" "-fvisibility" "default" "-stack-protector" "1" "-mstackrealign" "-fblocks" "-fobjc-runtime=macosx-10.7.0" "-fencode-extended-block-signature" "-fcxx-exceptions" "-fexceptions" "-fdiagnostics-show-option" "-vectorize-loops" "-vectorize-slp" "-x" "c++" "test0array.cc"


testvec: testvec.cc vectorCore.h
	clang++ -std=c++11 -stdlib=libc++ -O0 -g -o testvec testvec.cc

testLldb: testLldb.cc
	clang++ -std=c++11 -stdlib=libc++ -O0 -g  -o testLldb testLldb.cc


testSpecialization: testSpecialization.cc
	clang++ -fvisibility=default -std=c++11 -stdlib=libc++ -o testSpecialization testSpecialization.cc



clean:
	-rm testExport.o testExport
testExport: testExport.cc
	clang++ -fvisibility=default -std=c++11 -stdlib=libc++ -o testExport testExport.cc
	nm -mapv testExport | grep MyException
	echo is it external???
testTemplate9: testTemplate9.cc
	clang++ -std=c++11 -stdlib=libc++ -O0 -g  -o testTemplate9 testTemplate9.cc

testTemplate8: testTemplate8.cc
	clang++ -std=c++11 -stdlib=libc++  -o testTemplate8 testTemplate8.cc

testTemplate8_working: testTemplate8_working.cc
	clang++ -std=c++11 -stdlib=libc++  -o testTemplate8_working testTemplate8_working.cc


testTemplate7: testTemplate7.cc
	clang++ -std=c++11 -stdlib=libc++  -o testTemplate7 testTemplate7.cc

testTemplate6_working: testTemplate6_working.cc
	clang++ -std=c++11 -stdlib=libc++  -o testTemplate6_working testTemplate6_working.cc

testTemplate6: testTemplate6.cc
	clang++ -std=c++11 -stdlib=libc++  -o testTemplate6 testTemplate6.cc

testTemplate5_working: testTemplate5_working.cc
	clang++ -std=c++11 -stdlib=libc++  -o testTemplate5_working testTemplate5_working.cc


testTemplate3: testTemplate3.cc
	clang++ -std=c++11 -stdlib=libc++  -o testTemplate3 testTemplate3.cc


testTemplate1: testTemplate1.cc
	clang++ -std=c++11 -stdlib=libc++  -o testTemplate1 testTemplate1.cc

testTemplate0: testTemplate0.cc
	clang++ -std=c++11 -stdlib=libc++  -o testTemplate0 testTemplate0.cc



testVarArgs: testVarArgs.cc
	clang++  -g -O3 -o testVarArgs testVarArgs.cc

testmpi: testmpi.cc
	mpicxx -g -v -o testmpi testmpi.cc -I$(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_INCLUDE_DIR) -std=c++11 -stdlib=libc++ -lboost_mpi -L$(CLASP_APP_RESOURCES_EXTERNALS_RELEASE_LIB_DIR)
	cp testmpi $(CLASP_APP_BIN_DIR)




testVariadic: testVariadic.cc
	clang++ -std=c++11 -stdlib=libc++ -o testVariadic testVariadic.cc



testNil:
	g++ -o testNil -g -ggdb -pipe -fPIC -D$(DEVELOPMENT_ENVIRONMENT) $(EXTERNAL_INCLUDES) testNil.cc
	./testNil

po:
	clang++ -o testProgramOptions -std=c++11 -stdlib=libc++ testProgramOptions.cc
