# Command to generate preprocessor output
#
#"/usr/lib/llvm-3.5/bin/clang" -cc1 -triple x86_64-pc-linux-gnu -emit-obj -disable-free -disable-llvm-verifier -main-file-name profile.cc -mrelocation-model static -fmath-errno -masm-verbose -mconstructor-aliases -munwind-tables -fuse-init-array -target-cpu x86-64 -target-linker-version 2.22 -momit-leaf-frame-pointer -v -v -gdwarf-4 -dwarf-column-info -coverage-file /usr/local/src/clasp/clasp/src/main/../../src/core/bin/boehm/clang-linux-3.5.0/release/link-static/profile.o -resource-dir /usr/lib/llvm-3.5/bin/../lib/clang/3.5.0 -D EXPAT -D INCLUDED_FROM_CLASP -D INHERITED_FROM_SRC -D NDEBUG -D READLINE -D USE_BOEHM -D USE_CLASP_DYNAMIC_CAST -D USE_STATIC_ANALYZER_GLOBAL_SYMBOLS -D _ADDRESS_MODEL_32 -D _RELEASE_BUILD -D _TARGET_OS_LINUX -D __STDC_CONSTANT_MACROS -D __STDC_FORMAT_MACROS -D __STDC_LIMIT_MACROS -I ../../src -I ../../src/core -I ../../src/core/bin/boehm/clang-linux-3.5.0/release/link-static -I /usr/local/src/clasp/externals-clasp-build/common/include -I /usr/local/src/clasp/externals-clasp-build/release/include -internal-isystem /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/c++/4.9 -internal-isystem /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/x86_64-linux-gnu/c++/4.9 -internal-isystem /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/x86_64-linux-gnu/c++/4.9 -internal-isystem /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/c++/4.9/backward -internal-isystem /usr/local/include -internal-isystem /usr/lib/llvm-3.5/bin/../lib/clang/3.5.0/include -internal-externc-isystem /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/include -internal-externc-isystem /usr/include/x86_64-linux-gnu -internal-externc-isystem /include -internal-externc-isystem /usr/include -O3 -Warray-bounds -Wgnu-array-member-paren-init -Wno-deprecated-register -Wno-unused-local-typedef -Wno-unused-variable -Wno-inline -Wall -Warray-bounds -Wgnu-array-member-paren-init -Wno-deprecated-register -Wno-unused-local-typedef -Wno-unused-variable -std=c++11 -fdeprecated-macro -fdebug-compilation-dir /usr/local/src/clasp/clasp/src/main -ferror-limit 19 -fmessage-length 0 -fvisibility default -mstackrealign -fobjc-runtime=gcc -fcxx-exceptions -fexceptions -fdiagnostics-show-option -vectorize-loops -vectorize-slp -o profile.i -x c++ -E ../../src/core/profile.cc


# Command to compile file and generate error
#
"/usr/lib/llvm-3.5/bin/clang" -cc1 -triple x86_64-pc-linux-gnu -emit-obj -disable-free -disable-llvm-verifier -main-file-name profile.cc -mrelocation-model static -fmath-errno -masm-verbose -mconstructor-aliases -munwind-tables -fuse-init-array -target-cpu x86-64 -target-linker-version 2.22 -momit-leaf-frame-pointer -v -v -gdwarf-4 -dwarf-column-info -coverage-file /usr/local/src/clasp/clasp/src/main/../../src/core/bin/boehm/clang-linux-3.5.0/release/link-static/profile.o -resource-dir /usr/lib/llvm-3.5/bin/../lib/clang/3.5.0 -D EXPAT -D INCLUDED_FROM_CLASP -D INHERITED_FROM_SRC -D NDEBUG -D READLINE -D USE_BOEHM -D USE_CLASP_DYNAMIC_CAST -D USE_STATIC_ANALYZER_GLOBAL_SYMBOLS -D _ADDRESS_MODEL_32 -D _RELEASE_BUILD -D _TARGET_OS_LINUX -D __STDC_CONSTANT_MACROS -D __STDC_FORMAT_MACROS -D __STDC_LIMIT_MACROS -I ../../src -I ../../src/core -I ../../src/core/bin/boehm/clang-linux-3.5.0/release/link-static -I /usr/local/src/clasp/externals-clasp-build/common/include -I /usr/local/src/clasp/externals-clasp-build/release/include -internal-isystem /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/c++/4.9 -internal-isystem /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/x86_64-linux-gnu/c++/4.9 -internal-isystem /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/x86_64-linux-gnu/c++/4.9 -internal-isystem /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/c++/4.9/backward -internal-isystem /usr/local/include -internal-isystem /usr/lib/llvm-3.5/bin/../lib/clang/3.5.0/include -internal-externc-isystem /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/include -internal-externc-isystem /usr/include/x86_64-linux-gnu -internal-externc-isystem /include -internal-externc-isystem /usr/include -O3 -Warray-bounds -Wgnu-array-member-paren-init -Wno-deprecated-register -Wno-unused-local-typedef -Wno-unused-variable -Wno-inline -Wall -Warray-bounds -Wgnu-array-member-paren-init -Wno-deprecated-register -Wno-unused-local-typedef -Wno-unused-variable -std=c++11 -fdeprecated-macro -fdebug-compilation-dir /usr/local/src/clasp/clasp/src/main -ferror-limit 19 -fmessage-length 0 -fvisibility default -mstackrealign -fobjc-runtime=gcc -fcxx-exceptions -fexceptions -fdiagnostics-show-option -vectorize-loops -vectorize-slp -o profile.o -x c++ ../../src/core/profile.cc




# Output from original build with error
#
# warning: unknown warning option '-Wno-unused-local-typedef' [-Wunknown-warning-option]
# warning: unknown warning option '-Wno-unused-local-typedef' [-Wunknown-warning-option]
# clang -cc1 version 3.5.0 based upon LLVM 3.5.0 default target x86_64-pc-linux-gnu
# ignoring nonexistent directory "/include"
# ignoring duplicate directory "/usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/x86_64-linux-gnu/c++/4.9"
# #include "..." search starts here:
# #include <...> search starts here:
#  ../../src
#  ../../src/core
#  ../../src/core/bin/boehm/clang-linux-3.5.0/release/link-static
#  /usr/local/src/clasp/externals-clasp-build/common/include
#  /usr/local/src/clasp/externals-clasp-build/release/include
#  /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/c++/4.9
#  /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/x86_64-linux-gnu/c++/4.9
#  /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/c++/4.9/backward
#  /usr/local/include
#  /usr/lib/llvm-3.5/bin/../lib/clang/3.5.0/include
#  /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/include
#  /usr/include/x86_64-linux-gnu
#  /usr/include
# End of search list.
#
# In file included from ../../src/core/profile.cc:39:
# In file included from ../../src/core/clasp_gmpxx.h:32:
# In file included from /usr/local/src/clasp/externals-clasp-build/common/include/gmpxx.h:41:
# In file included from /usr/local/src/clasp/externals-clasp-build/common/include/gmp.h:51:
# /usr/bin/../lib/gcc/x86_64-linux-gnu/4.9/../../../../include/c++/4.9/cstddef:51:11: error: no member named 'max_align_t' in the global namespace
#   using ::max_align_t;
#         ~~^
# 2 warnings and 1 error generated.
#
#   "/usr/bin/clang-3.5" -c -x c++ -O3 -gdwarf-4 -Warray-bounds -Wgnu-array-member-paren-init -Wno-deprecated-register -Wno-unused-local-typedef -Wno-unused-variable -fvisibility=default -std=c++11 -v -O3 -gdwarf-4 -O3 -Wno-inline -Wall -O3 -gdwarf-4 -Warray-bounds -Wgnu-array-member-paren-init -Wno-deprecated-register -Wno-unused-local-typedef -Wno-unused-variable -fvisibility=default -std=c++11 -v -O3 -gdwarf-4 -DEXPAT -DINCLUDED_FROM_CLASP -DINHERITED_FROM_SRC -DNDEBUG -DREADLINE -DUSE_BOEHM -DUSE_CLASP_DYNAMIC_CAST -DUSE_STATIC_ANALYZER_GLOBAL_SYMBOLS -D_ADDRESS_MODEL_32 -D_RELEASE_BUILD -D_TARGET_OS_LINUX -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -I"../../src" -I"../../src/core" -I"../../src/core/bin/boehm/clang-linux-3.5.0/release/link-static" -I"/usr/local/src/clasp/externals-clasp-build/common/include" -I"/usr/local/src/clasp/externals-clasp-build/release/include" -o "../../src/core/bin/boehm/clang-linux-3.5.0/release/link-static/profile.o" "../../src/core/profile.cc"
#
# ...failed compile.c++.without-pth ../../src/core/bin/boehm/clang-linux-3.5.0/release/link-static/profile.o...
#
