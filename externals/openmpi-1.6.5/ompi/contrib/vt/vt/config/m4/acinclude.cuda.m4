AC_DEFUN([ACVT_CUDA],
[
	cuda_error="no"
	cudart_error="no"
	have_cuda="no"
	have_cudart="no"

	CUDATKDIR=
	CUDATKINCDIR=
	CUDATKLIBDIR=
	CUDALIB=
	CUDARTLIB=

	AC_ARG_VAR(NVCC, [NVIDIA CUDA compiler command])

	AC_ARG_WITH(cuda-dir,
		AC_HELP_STRING([--with-cuda-dir=CUDATKDIR],
		[give the path for CUDA Toolkit, default: /usr/local/cuda]),
	[CUDATKDIR="$withval/"], [CUDATKDIR="/usr/local/cuda/"])

	AC_ARG_WITH(cuda-inc-dir,
		AC_HELP_STRING([--with-cuda-inc-dir=CUDATKINCDIR],
		[give the path for CUDA-Toolkit-include files, default: CUDATKDIR/include]),
	[CUDATKINCDIR="-I$withval/"],
	[AS_IF([test x"$CUDATKDIR" != x], [CUDATKINCDIR="-I$CUDATKDIR"include/])])

	AC_ARG_WITH(cuda-lib-dir,
		AC_HELP_STRING([--with-cuda-lib-dir=CUDATKLIBDIR],
		[give the path for CUDA-Toolkit-libraries, default: CUDATKDIR/lib64]),
	[CUDATKLIBDIR="-L$withval/"],
	[AS_IF([test x"$CUDATKDIR" != x], [CUDATKLIBDIR="-L$CUDATKDIR"lib64/])])

	AC_ARG_WITH(cuda-lib,
		AC_HELP_STRING([--with-cuda-lib=CUDALIB], [use given CUDA driver library, default: -lcuda]),
	[CUDALIB="$withval"])

	AC_ARG_WITH(cudart-lib,
		AC_HELP_STRING([--with-cudart-lib=CUDARTLIB], [use given CUDA runtime library, default: -lcudart]),
	[CUDARTLIB="$withval"])

	AS_IF([test x"$cuda_error" = "xno"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $CUDATKINCDIR"
		AC_CHECK_HEADER([cuda.h], [],
		[
			AC_MSG_NOTICE([error: no cuda.h found; check path for CUDA Toolkit first...])
			cuda_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$CUDALIB" = x -a x"$cuda_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $CUDATKLIBDIR -lcuda"
		AC_MSG_CHECKING([whether linking with -lcuda works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); CUDALIB=-lcuda],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$CUDALIB" = x -a x"$cuda_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libcuda found; check path for CUDA Toolkit first...])
		cuda_error="yes"
	])

	AS_IF([test x"$cudart_error" = "xno"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $CUDATKINCDIR"
		AC_CHECK_HEADER([cuda_runtime_api.h], [],
		[
			AC_MSG_NOTICE([error: no cuda_runtime_api.h found; check path for CUDA Toolkit first...])
			cudart_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$CUDARTLIB" = x -a x"$cudart_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $CUDATKLIBDIR -lcudart"
		AC_MSG_CHECKING([whether linking with -lcudart works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); CUDARTLIB=-lcudart],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$CUDARTLIB" = x -a x"$cudart_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libcudart found; check path for CUDA Toolkit first...])
		cudart_error="yes"
	])

	AS_IF([test x"$cudart_error" = "xno"],
	[
		AC_MSG_CHECKING([whether CUDA runtime version >= 3.0])

		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $CUDATKINCDIR"
		AC_TRY_COMPILE([#include "cuda_runtime_api.h"],
		[
#ifndef CUDART_VERSION
#  error "CUDART_VERSION not defined"
#elif CUDART_VERSION < 3000
#  error "CUDART_VERSION < 3000"
#endif
		],
		[AC_MSG_RESULT([yes])],
		[
			AC_MSG_RESULT([no])
			AC_MSG_NOTICE([error: CUDA runtime version could not be determined and/or is incompatible (< 3.0)
See \`config.log' for more details.])
			cudart_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$cudart_error" = "xno"],
	[
		AC_CHECK_PROG(NVCC, nvcc, nvcc, , [$PATH$PATH_SEPARATOR$CUDATKDIR"bin/"])
		have_cudart="yes"
	])

	AS_IF([test x"$cuda_error" = "xno"],
	[
		have_cuda="yes"
	],
	[
dnl		if no CUDA found, remove content of CUDATKLIBDIR to prevent adding them
dnl		to the linker flags when using the VT compiler wrappers
		CUDATKLIBDIR=
	])

	AC_SUBST(CUDATKDIR)
	AC_SUBST(CUDATKINCDIR)
	AC_SUBST(CUDATKLIBDIR)
	AC_SUBST(CUDALIB)
	AC_SUBST(CUDARTLIB)
])
