#
#   Order of clang and LLVM libraries were determined here
#   Run this script and paste the output into jamfile.jam.lib
#
clangOrder = [
# Things added by hand because clang doesn't use them
"clangASTMatchers",
"clangDynamicASTMatchers",
"clangTooling",
# libraries clang needs
"clangFrontendTool",
"clangFrontend",
"clangDriver",
"clangSerialization",
"clangCodeGen",
"clangParse",
"clangSema",
"clangStaticAnalyzerFrontend",
"clangStaticAnalyzerCheckers",
"clangStaticAnalyzerCore",
"clangARCMigrate",
"clangRewriteFrontend",
"clangRewriteCore",
"clangAnalysis",
"clangEdit",
"clangAST",
"clangLex",
"clangBasic" ]



llvmOrder = [
#
# These are not used by the clang build so they must be added separately
#
"LLVMJIT",
"LLVMMCJIT",
"LLVMInterpreter",
"LLVMExecutionEngine",
"LLVMRuntimeDyld",
#
# These are used by clang
#
"LLVMProfileData",
"LLVMOption",
"LLVMObjCARCOpts",
"LLVMLinker",
"LLVMIRReader",
"LLVMipo",
"LLVMVectorize",
"LLVMInstrumentation",
"LLVMBitWriter",
"LLVMAsmParser",
"LLVMX86Disassembler",
"LLVMX86AsmParser",
"LLVMX86CodeGen",
"LLVMSelectionDAG",
"LLVMAsmPrinter",
"LLVMMCParser",
"LLVMCodeGen",
"LLVMScalarOpts",
"LLVMInstCombine",
"LLVMTransformUtils",
"LLVMipa",
"LLVMAnalysis",
"LLVMTarget",
"LLVMX86Desc",
"LLVMX86Info",
"LLVMX86AsmPrinter",
"LLVMMC",
"LLVMObject",
"LLVMBitReader",
"LLVMCore",
"LLVMX86Utils",
"LLVMSupport"
]



clangOrder.reverse()
print("# clangOrder = %s" % clangOrder)
for x in clangOrder:
    print("lib lib%s : : <search>$(APPRES-EXTERNALS-RELEASE-LIB) <name>%s <variant>release <link>static ;" % (x,x))    
    print("lib lib%s : : <search>$(APPRES-EXTERNALS-DEBUG-LIB) <name>%s <variant>debug <link>static ;" % (x,x))

print("alias clang_lib :")
for x in clangOrder:
    print("    lib%s" % x)
print("   : <link>static ;")


llvmOrder.reverse()
print("# llvmOrder = %s" % llvmOrder )
for x in llvmOrder:
    print("lib lib%s : : <search>$(APPRES-EXTERNALS-RELEASE-LIB) <name>%s <variant>release <link>static ;" % (x,x))    
    print("lib lib%s : : <search>$(APPRES-EXTERNALS-DEBUG-LIB) <name>%s <variant>debug <link>static ;" % (x,x))
print("alias llvm_lib :")
for x in llvmOrder:
    print("    lib%s" % x)
print("  : <link>static ;")
