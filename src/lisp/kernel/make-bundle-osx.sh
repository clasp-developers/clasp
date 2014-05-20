#!/bin/bash
echo Creating bundle: $1
echo Linking bitcode files $1-wrapper.lnk + `cat $1-wrapper.lnk`
llvm-link -o $1.bc `cat $1-wrapper.lnk` $1-wrapper.bc
echo Compiling bitcode file $1-wrapper.bc
llc -filetype=obj $1-wrapper.bc -o=$1.o
echo Linking library
# use -flat_namespace -undefined suppress ????
ld $1.o -flat_namespace -undefined warning -bundle -L"/Library/Frameworks/EPD64.framework/Versions/current/lib" -L"/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/externals/release/lib"  -L"/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/externals/common/lib"  "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libllvmo_opt.dylib" "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libgctools_opt.dylib" "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libcffi_opt.dylib" "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libcore_opt.dylib"   -lgmpxx -lgmp -lncurses -lreadline -lz -lexpat -lboost_python -lboost_iostreams -lboost_system -lboost_program_options -lboost_serialization -lboost_date_time -lboost_thread -lboost_regex -lboost_filesystem -lLLVM-3.3svn -o "$1.bundle"
