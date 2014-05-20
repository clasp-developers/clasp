#! /bin/bash
ld _image-wrapper.o `cat _image.object-files` -bundle  -o "$1.bundle"  -undefined dynamic_lookup

#-flat_namespace
# -bundle_loader $HOME/Development/cando/build/cando.app/Contents/MacOS/brcl_d

#-L"/Library/Frameworks/EPD64.framework/Versions/current/lib"  -L"/Users/meister/Development/cando/build/cando.app/Contents/Resources/externals/release/lib"  "/Users/meister/Development/cando/build/cando.app/Contents/Resources/lib/libllvmo_dbg.dylib" "/Users/meister/Development/cando/build/cando.app/Contents/Resources/lib/libcffi_dbg.dylib" "/Users/meister/Development/cando/build/cando.app/Contents/Resources/lib/libcore_dbg.dylib"  -lpython2.7 -lpython2.7 -lgmpxx -lgmp -lncurses -lreadline -lz -lexpat -lboost_python -lboost_iostreams -lboost_system -lboost_program_options -lboost_serialization -lboost_date_time -lboost_thread -lboost_regex -lboost_filesystem -lLLVM-3.3svn
