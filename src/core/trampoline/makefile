
DIR=/usr/local/opt/llvm@14/bin
CLANG=$(DIR)/clang++
DIS=$(DIR)/llvm-dis
all:
	$(CLANG) -c -emit-llvm -O3 -g -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer trampoline.cc
	$(DIS) -o=trampoline-raw.ll trampoline.bc
	tail --lines=+5 trampoline-raw.ll >trampoline.ll


#	$(CLANG) -c -emit-llvm -O3 -g -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer trampoline.cc
