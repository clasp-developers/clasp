

all:
	clang++-14 -c -emit-llvm -O3 -g -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer trampoline.cc
	llvm-dis-14 trampoline.bc


#	clang++-14 -c -emit-llvm -O3 -g -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer trampoline.cc
