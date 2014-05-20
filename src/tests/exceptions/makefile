


include	$(CANDO_HOME)/makefile.inc


INC_NEEDS = llvmoExpose.generated.cc llvmoExpose.generated.h
NEEDS = initClasses.inc $(INC_NEEDS) symbols_scraped.inc




debug_te:
	candoLisp_d tc.lsp
	llvm-dis <te.bc -o=te.ll
	opt -analyze te.bc -dot-cfg
	./dotAll.sh cfg*.dot
	echo "Open the cfg.repl.svg files"


cproto:
	"clang++" -S -emit-llvm -x c++ -O0 -g -O0 -fno-inline -Wall -g -DEXPAT -DNDEBUG -DSOURCE_DEBUG -D_DEBUG -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -I"." -I"/Users/meister/Development/cando/build/include" -I"/Users/meister/Development/cando/build/include/boost" -I"/Users/meister/Development/cando/src" -I"/Users/meister/Development/cando/src/core" -I"/Users/meister/Development/cando/build" -I"/Users/meister/Development/cando/build/llvm/include" -I"/Users/meister/Development/cando/externals/src/boost_1_49_0" `llvm-config --cppflags --ldflags --libs core jit native` \
-lprofiler -lprofiler -L"../../build/lib" -L"/Users/meister/Development/cando/build/cando.app/Contents/Resources/lib" -lLLVMRuntimeDyld -lLLVMMCParser -lLLVMAsmPrinter -lLLVMSelectionDAG -lLLVMTransformUtils -lLLVMScalarOpts -lLLVMAnalysis -lLLVMTarget -lLLVMMC -lLLVMExecutionEngine -lLLVMJIT -lLLVMSupport -lLLVMCore -lreadline -lz -lexpat -lboost_python -lboost_iostreams -lboost_system -lboost_program_options -lboost_serialization -lboost_date_time -lboost_thread -lboost_regex -lboost_filesystem -lgmpxx -lgmp -lLLVMX86AsmPrinter -lLLVMX86Desc -lLLVMX86AsmParser -lLLVMX86Utils -lLLVMX86Info -lLLVMX86Disassembler -lLLVMX86CodeGen -lLLVMObject -lLLVMipa -lLLVMInstCombine -lLLVMCodeGen -lllvmoPackage_d -lcffiPackage_d -lcore_d  -o "cproto.ll" "cproto.cc"
	make cproto_as
cproto_as:
	llvm-as <cproto.ll -o=cproto.bc
	opt -analyze cproto.bc -dot-cfg
	./dotAll.sh cfg.*.dot


