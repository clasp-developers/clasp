!**
!* VampirTrace
!* http://www.tu-dresden.de/zih/vampirtrace
!*
!* Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
!*
!* Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
!*                          Centre, Federal Republic of Germany
!*
!* See the file COPYING in the package base directory for details
!**

#define VT_COUNT_DEFGROUP       -1

#define VT_COUNT_TYPE_INTEGER    11
#define VT_COUNT_TYPE_INTEGER8   12
#define VT_COUNT_TYPE_REAL       13
!* vvv - equal to C/C++ type
#define VT_COUNT_TYPE_DOUBLE     4

#if (defined(VTRACE)) && !(defined(VTRACE_NO_COUNT))
# define VT_COUNT_GROUP_DEF(n, gi) call VT_User_count_group_def__((n), gi)
# define VT_COUNT_DEF(n, u, t, gi, ci) call VT_User_count_def__((n), (u), (t), (gi), ci)
# define VT_COUNT_INTEGER_VAL(i, v) call VT_User_count_integer_val__((i), (v))
# define VT_COUNT_INTEGER8_VAL(i, v) call VT_User_count_integer8_val__((i), (v))
# define VT_COUNT_REAL_VAL(i, v) call VT_User_count_real_val__((i), (v))
# define VT_COUNT_DOUBLE_VAL(i, v) call VT_User_count_double_val__((i), (v))
#else
# define VT_COUNT_GROUP_DEF(n, gi)
# define VT_COUNT_DEF(n, u, t, gi, ci)
# define VT_COUNT_INTEGER_VAL(i, v)
# define VT_COUNT_INTEGER8_VAL(i, v)
# define VT_COUNT_REAL_VAL(i, v)
# define VT_COUNT_DOUBLE_VAL(i, v)
#endif
