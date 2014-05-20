


include	$(CANDO_HOME)/makefile.inc


ECL = ecl

INC_NEEDS = llvmoExpose.generated.cc llvmoExpose.generated.h
NEEDS = initClasses.inc $(INC_NEEDS) symbols_scraped.inc



sad subAllDebug: $(NEEDS)
	make scrape
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single  -j $(COMPILE_PROCESSORS) install_dbg
	make subBundle

sa subAll: $(NEEDS)
	make scrape
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single  -j $(COMPILE_PROCESSORS) install_dbg install_opt
	make subBundle

san: $(NEEDS)
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single install_dbg install_opt -j $(COMPILE_PROCESSORS) -a -n >_bjam.txt

sb subBundle:
	cp libs/[a-z]* $(CANDO_APP_LIB_DIR)
bjd:
	$(BJAM) toolset=$(BUILD_TOOLSET) link=$(BUILD_LINK) threading=single debug -j $(COMPILE_PROCESSORS)






toy:	toy.cc
	"clang++" -x c++ -O0 -g -O0 -fno-inline -Wall -g -DEXPAT -DNDEBUG -DSOURCE_DEBUG -D_DEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -I"." -I"../../build/include" -I"../../build/include/boost" -I"../../src" -I"../core" -I"/Users/meister/Development/cando/build" -I"/Users/meister/Development/cando/build/llvm/include" -I"/Users/meister/Development/cando/externals/src/boost_1_49_0" `llvm-config --cppflags --ldflags --libs core jit native` \
-lprofiler -lprofiler -L"../../build/lib" -L"/Users/meister/Development/cando/build/cando.app/Contents/Resources/lib" -lLLVMRuntimeDyld -lLLVMMCParser -lLLVMAsmPrinter -lLLVMSelectionDAG -lLLVMTransformUtils -lLLVMScalarOpts -lLLVMAnalysis -lLLVMTarget -lLLVMMC -lLLVMExecutionEngine -lLLVMJIT -lLLVMSupport -lLLVMCore -lreadline -lz -lexpat -lboost_python -lboost_iostreams -lboost_system -lboost_program_options -lboost_serialization -lboost_date_time -lboost_thread -lboost_regex -lboost_filesystem -lgmpxx -lgmp -lLLVMX86AsmPrinter -lLLVMX86Desc -lLLVMX86AsmParser -lLLVMX86Utils -lLLVMX86Info -lLLVMX86Disassembler -lLLVMX86CodeGen -lLLVMObject -lLLVMipa -lLLVMInstCombine -lLLVMCodeGen -lllvmoPackage_d -lcffiPackage_d -lcore_d  -o "toy" "toy.cc"

toy.ir: toy.cc
	"clang++" -S -emit-llvm -x c++ -O0 -g -O0 -fno-inline -Wall -g -DEXPAT -DNDEBUG -DSOURCE_DEBUG -D_DEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -I"." -I"../../build/include" -I"../../build/include/boost" -I"../../src" -I"../core" -I"/Users/meister/Development/cando/build" -I"/Users/meister/Development/cando/build/llvm/include" -I"/Users/meister/Development/cando/externals/src/boost_1_49_0" `llvm-config --cppflags --ldflags --libs core jit native` \
-lprofiler -lprofiler -L"../../build/lib" -L"/Users/meister/Development/cando/build/cando.app/Contents/Resources/lib" -lLLVMRuntimeDyld -lLLVMMCParser -lLLVMAsmPrinter -lLLVMSelectionDAG -lLLVMTransformUtils -lLLVMScalarOpts -lLLVMAnalysis -lLLVMTarget -lLLVMMC -lLLVMExecutionEngine -lLLVMJIT -lLLVMSupport -lLLVMCore -lreadline -lz -lexpat -lboost_python -lboost_iostreams -lboost_system -lboost_program_options -lboost_serialization -lboost_date_time -lboost_thread -lboost_regex -lboost_filesystem -lgmpxx -lgmp -lLLVMX86AsmPrinter -lLLVMX86Desc -lLLVMX86AsmParser -lLLVMX86Utils -lLLVMX86Info -lLLVMX86Disassembler -lLLVMX86CodeGen -lLLVMObject -lLLVMipa -lLLVMInstCombine -lLLVMCodeGen -lllvmoPackage_d -lcffiPackage_d -lcore_d  -o "toy" "toy.cc"


cproto:
	"clang++" -S -emit-llvm -x c++ -O0 -g -O0 -fno-inline -Wall -g -DEXPAT -DNDEBUG -DSOURCE_DEBUG -D_DEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -I"." -I"../../build/include" -I"../../build/include/boost" -I"../../src" -I"../core" -I"/Users/meister/Development/cando/build" -I"/Users/meister/Development/cando/build/llvm/include" -I"/Users/meister/Development/cando/externals/src/boost_1_49_0" `llvm-config --cppflags --ldflags --libs core jit native` \
-lprofiler -lprofiler -L"../../build/lib" -L"/Users/meister/Development/cando/build/cando.app/Contents/Resources/lib" -lLLVMRuntimeDyld -lLLVMMCParser -lLLVMAsmPrinter -lLLVMSelectionDAG -lLLVMTransformUtils -lLLVMScalarOpts -lLLVMAnalysis -lLLVMTarget -lLLVMMC -lLLVMExecutionEngine -lLLVMJIT -lLLVMSupport -lLLVMCore -lreadline -lz -lexpat -lboost_python -lboost_iostreams -lboost_system -lboost_program_options -lboost_serialization -lboost_date_time -lboost_thread -lboost_regex -lboost_filesystem -lgmpxx -lgmp -lLLVMX86AsmPrinter -lLLVMX86Desc -lLLVMX86AsmParser -lLLVMX86Utils -lLLVMX86Info -lLLVMX86Disassembler -lLLVMX86CodeGen -lLLVMObject -lLLVMipa -lLLVMInstCombine -lLLVMCodeGen -lllvmoPackage_d -lcffiPackage_d -lcore_d  -o "cproto.ll" "cproto.cc"
	llvm-as <cproto.ll -o=cproto.bc
	opt -analyze cproto.bc -dot-cfg
	./testException/dotAll.sh cfg.*.dot


demo:	demo.cc
	"clang++" -faddress-sanitizer -O1 -fno-omit-frame-pointer -g -fno-inline -Wall -g   -o "demo" "demo.cc"


#	clang++ -g -O0 toy.cc `llvm-config --cppflags --ldflags --libs core jit native` -O3 -o toy


subMeta sm:
	rm -f *.inc
	touch symbols_scraped.inc
	make llvmoExpose.generated.cc
	make mi
	make cscrape

initClasses.inc : *.h llvmoExpose.generated.h
	make mi

llvmoExpose.generated.cc: llvmoExpose.lisp ../common/expose.lisp
	$(ECL) -shell llvmoExpose.lisp

llvmoExpose.generated.h: llvmoExpose.lisp ../common/expose.lisp
	$(ECL) -shell llvmoExpose.lisp



relink:
	rm libs/[a-z]*
	make sa

subClean sc:
	rm -rf bin/*

totalClean:
	rm -rf bin install_release



llvm_config:
	llvm-config --cppflags --ldflags --libs core
