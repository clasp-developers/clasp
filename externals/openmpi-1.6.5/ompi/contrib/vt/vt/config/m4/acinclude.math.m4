AC_DEFUN([ACVT_MATH],
[
	math_error="no"
	have_math="no"

	MATHLIB=

	AC_CHECK_HEADER([math.h], [],
	[
		AC_MSG_NOTICE([error: no math.h found; check path for libmath package first...])
		math_error="yes"
	])

	AS_IF([test x"$math_error" = "xno"],
	[
		AC_MSG_CHECKING([whether we need to link -lm to get math functions])
		AC_TRY_LINK([#include <math.h>],
[
  volatile double c, x = 47.11;
  c = ceil(x);
],
		[AC_MSG_RESULT([no])], [AC_MSG_RESULT([yes]); MATHLIB=-lm])
	])

	AS_IF([test x"$math_error" = "xno"], [have_math="yes"])

	AC_SUBST(MATHLIB)
])

