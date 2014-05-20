#! /bin/bash

echo Compiling _image-wrapper.o
llc -filetype=obj _image-wrapper.bc -o=_image-wrapper.o
for x in `cat $1-wrapper.lnk`; do
    obj=${x%.*}.o
    echo "Compiling $obj"
    llc -filetype=obj $x -o=$obj
    objects+=($obj)
done
ld -v $1-wrapper.o ${objects[@]} -macosx_version_min 10.7 -flat_namespace -undefined warning -bundle  -L"/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/externals/release/lib"  -L"/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/externals/common/lib"  "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libllvmo_opt.dylib" "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libgctools_opt.dylib" "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libcffi_opt.dylib" "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libcore_opt.dylib"   -lgmpxx -lgmp -lncurses -lreadline -lz -lexpat  -lboost_iostreams -lboost_system -lboost_program_options -lboost_serialization -lboost_date_time -lboost_thread -lboost_regex -lboost_filesystem -lLLVM-3.3svn -o "$1.bundle"


ld -v $1-wrapper.o ${objects[@]} -macosx_version_min 10.7 -flat_namespace -undefined warning -bundle  -L"/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/externals/release/lib"  -L"/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/externals/common/lib"  "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libllvmo_opt.dylib" "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libgctools_opt.dylib" "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libcffi_opt.dylib" "/Users/meister/Development/cando/brcl/build/cando.app/Contents/Resources/lib/libcore_opt.dylib"   -lgmpxx -lgmp -lncurses -lreadline -lz -lexpat  -lboost_iostreams -lboost_system -lboost_program_options -lboost_serialization -lboost_date_time -lboost_thread -lboost_regex -lboost_filesystem -lLLVM-3.3svn -o "$1.bundle"
