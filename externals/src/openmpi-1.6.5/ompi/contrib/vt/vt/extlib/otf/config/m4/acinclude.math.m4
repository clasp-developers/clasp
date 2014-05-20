AC_DEFUN([CHECK_MATH],
[
    LIBS="$LIBS -lm"
    AC_MSG_CHECKING([whether linking with -lm works])
    AC_TRY_LINK([],[],
    [AC_MSG_RESULT([yes]); math_lib=-lm],[AC_MSG_RESULT([no])])
    LIBS=$sav_LIBS

    AS_IF([test x"$math_lib" != x],
    [
        AC_MSG_CHECKING([whether we need to link -lm to get math functions])
        AC_TRY_LINK([#include <math.h>],
[
  volatile double c, x = 47.11;
  c = ceil(x);
],
        [AC_MSG_RESULT([no])], [AC_MSG_RESULT([yes]); MATHLIB=$math_lib])
    ])

    AC_SUBST(MATHLIB)
])
