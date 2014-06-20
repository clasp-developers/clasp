/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_USER_H
# error "vt_user_region.h should only be included from vt_user.h"
#endif /* _VT_USER_H */

#ifndef _VT_USER_REGION_H
#define _VT_USER_REGION_H

__VT_EXTERN_DECL void VT_User_start__(const char* name, const char* file,
                                      int lno);
__VT_EXTERN_DECL void VT_User_end__(const char* name);
__VT_EXTERN_DECL void VT_User_start2__(unsigned int rid);
__VT_EXTERN_DECL void VT_User_end2__(unsigned int rid);
__VT_EXTERN_DECL unsigned int VT_User_def__(const char* name, const char* group,
                                            const char* file, int lno);

#ifdef __cplusplus
  char vt_tracer_spec__(unsigned int);
  int vt_tracer_spec__(const char*);

  template<int> struct VT_Tracer {
    VT_Tracer(const char* r, const char* f, int l) __VT_NOINST_ATTR;
    ~VT_Tracer() __VT_NOINST_ATTR;
    const char* n;
  };
  template<> inline VT_Tracer<sizeof(int)>::VT_Tracer(const char* r,
    const char* f, int l) : n(r) { VT_User_start__(n, f, l); }
  template<> inline VT_Tracer<sizeof(int)>::~VT_Tracer() { VT_User_end__(n); }

  template<> struct VT_Tracer<1> {
    VT_Tracer(unsigned int r, const char*, int) __VT_NOINST_ATTR;
    ~VT_Tracer() __VT_NOINST_ATTR;
    unsigned int i;
  };
  inline VT_Tracer<1>::VT_Tracer(unsigned int r, const char*, int)
    : i(r) { VT_User_start2__(i); }
  inline VT_Tracer<1>::~VT_Tracer() { VT_User_end2__(i); }
#endif /* __cplusplus */

#if (defined(VTRACE)) && !(defined(VTRACE_NO_REGION))

# define VT_USER_START(n) VT_User_start__((n), __FILE__, __LINE__)
# define VT_USER_END(n) VT_User_end__((n))
# define VT_USER_START2(i) VT_User_start2__((i))
# define VT_USER_END2(i) VT_User_end2__(i)
# define VT_USER_DEF(n, g, f, l) VT_User_def__((n), (g), (f), (l))
# ifdef __cplusplus
#   define VT_TRACER(n_i) VT_Tracer<sizeof(vt_tracer_spec__(n_i))> \
      vt_tracer__((n_i), __FILE__, __LINE__);
# endif /* __cplusplus */

#else /* VTRACE && !VTRACE_NO_REGION */

# define VT_USER_START(n)
# define VT_USER_END(n)
# define VT_USER_START2(i)
# define VT_USER_END2(i)
# define VT_USER_DEF(n, g, f, l) 0
# define VT_TRACER(n_i)

#endif /* VTRACE && !VTRACE_NO_REGION */

#endif /* _VT_USER_H */
