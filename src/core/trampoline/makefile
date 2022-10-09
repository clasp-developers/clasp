
UNAME=$(shell uname)

ifeq ($(UNAME), Linux)
DIR=/usr/bin
CLANG=$(DIR)/clang++-14
DIS=$(DIR)/llvm-dis-14
endif
ifeq ($(UNAME), Darwin)
DIR=/usr/local/opt/llvm@14/bin
CLANG=$(DIR)/clang++
DIS=$(DIR)/llvm-dis
endif

all:
	$(CLANG) -c -emit-llvm -O3 -g -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer trampoline.cc
	$(DIS) -o=trampoline-raw.ll trampoline.bc
	tail --lines=+5 trampoline-raw.ll >trampoline.ll


#	$(CLANG) -c -emit-llvm -O3 -g -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer trampoline.cc
