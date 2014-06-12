
get-submodules:
	(cd externals/src; git submodule init llvm)
	(cd externals/src; git submodule init clang)
	(cd externals/src; git submodule update llvm)
	(cd externals/src; git submodule update clang)

deps:
	(cd externals/src; make subAll;)


setup-deps:
	(cd externals/src; make setup)

