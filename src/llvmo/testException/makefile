

sad:
	make compile
	make analyze
	make dis
	make dot
	make open-svg

compile:
	clang++  -c -emit-llvm test_exception.cc

analyze:
	opt -analyze test_exception.o -dot-cfg

dis:
	llvm-dis <test_exception.o -o=test_exception.ll

dot:
	./dotAll.sh cfg.proto_*.dot

open-svg:
	open cfg.proto_*.svg annotated-*.svg
