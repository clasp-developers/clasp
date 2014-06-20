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

#ifndef _VT_LIBWRAP_H
#define _VT_LIBWRAP_H

#include <stdlib.h>

/* maximum number of shared libraries, which will be searched for the actual
   library functions */
#define VT_LIBWRAP_MAX_SHLIBS 10

/* default library wrapper attributes */
#define VT_LIBWRAP_ATTR_DEFAULT {                                             \
  0,        /* shlibs_num */                                                  \
  { NULL }, /* shlibs */                                                      \
  NULL,     /* func_group */                                                  \
  0,        /* libc */                                                        \
  0,        /* wait_for_init */                                               \
  NULL      /* init_func */                                                   \
}

/* library wrapper attributes with a pointer to an initializer function */
#define VT_LIBWRAP_ATTR_INITIALIZER(_init_func) {                             \
  0,         /* shlibs_num */                                                 \
  { NULL },  /* shlibs */                                                     \
  NULL,      /* func_group */                                                 \
  0,         /* libc */                                                       \
  0,         /* wait_for_init */                                              \
  _init_func /* init_func */                                                  \
}

/* miscellaneous constants */
#define VT_LIBWRAP_NULL NULL
#define VT_LIBWRAP_NOID -1

/* initialize a wrapper function
   It creates the library wrapper object, gets the pointer to the actual
   library function, and assigns an unique identifier to the function.
   It declares some additional variables, so it must be called right after
   the declaration section of a wrapper function.
   arguments:
     _lw       = library wrapper object
                 (VT_LIBWRAP_NULL indicates whose creation)
     _lwattr   = library wrapper attributes
     _func     = function name
     _rettype  = return type
     _argtypes = argument types
     _file     = source code location (file)
     _line     = source code location (line) */
#define VT_LIBWRAP_FUNC_INIT(_lw, _lwattr, _func, _rettype, _argtypes,        \
                             _file, _line)                                    \
  _VT_LIBWRAP_FUNC_INIT_DECL_VARS(_func, _rettype, _argtypes);                \
  if( _lw == VT_LIBWRAP_NULL ) {                                              \
    VTLibwrap_create(&_lw, &_lwattr);                                         \
  }                                                                           \
  if( VT_LIBWRAP_FUNC_PTR == VT_LIBWRAP_NULL ||                               \
      VT_LIBWRAP_FUNC_ID  == VT_LIBWRAP_NOID ) {                              \
    VTLibwrap_func_init(_lw, VT_LIBWRAP_FUNC_NAME, _file, _line,              \
                        (void**)(&VT_LIBWRAP_FUNC_PTR), &VT_LIBWRAP_FUNC_ID); \
  }

/* internal macro used within VT_LIBWRAP_FUNC_INIT to declare variables for
   storing the function name, the actual function pointer, and the unique
   function identifier */
#define _VT_LIBWRAP_FUNC_INIT_DECL_VARS(_func, _rettype, _argtypes)           \
  static const char* VT_LIBWRAP_FUNC_NAME = _func;                            \
  static _rettype (*VT_LIBWRAP_FUNC_PTR)_argtypes = VT_LIBWRAP_NULL;          \
  static int VT_LIBWRAP_FUNC_ID = VT_LIBWRAP_NOID

/* The following macros are only available inside a wrapper function after
   calling VT_LIBWRAP_FUNC_INIT. */

/* variable name of function name */
#define VT_LIBWRAP_FUNC_NAME _vtlw_funcname
/* variable name of pointer to the real function */
#define VT_LIBWRAP_FUNC_PTR _vtlw_funcptr
/* variable name of function identifier */
#define VT_LIBWRAP_FUNC_ID  _vtlw_funcid

/* trigger a function enter/leave event
   (similarly to the VampirTrace API calls VT_USER_START/END)
   argument:
     _lw = library wrapper object */
#define VT_LIBWRAP_FUNC_START(_lw) VTLibwrap_func_start(_lw, VT_LIBWRAP_FUNC_ID)
#define VT_LIBWRAP_FUNC_END(_lw) VTLibwrap_func_end(_lw, VT_LIBWRAP_FUNC_ID)

/* call the real function
   arguments:
     _lw = library wrapper object
     _args = function arguments */
#define VT_LIBWRAP_FUNC_CALL(_lw, _args) (*VT_LIBWRAP_FUNC_PTR)_args

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* typedef for "opaque" library wrapper object */
typedef struct VTLibwrap_struct VTLibwrap;

/* typedef for library wrapper attributes */
typedef struct VTLibwrapAttr_struct VTLibwrapAttr;

/* typedef for attribute initializer function pointer */
typedef void(*VTLibwrapAttrInitFunc)(VTLibwrapAttr*);

/* data structure for the library wrapper attributes */
struct VTLibwrapAttr_struct
{
  /* Number of shared libraries which will be searched for the actual library
     functions. If 0 is specified, the addresses of the functions will be
     searched in the shared libraries linked to the application, in order,
     starting after the VampirTrace library. */
  int   shlibs_num;

  /* Array of pathnames to the shared libraries which will be searched for the
     actual library functions. */
  char* shlibs[VT_LIBWRAP_MAX_SHLIBS];

  /* Separate function group which will be assigned to all of the wrapped
     functions. If no group specified (NULL), each wrapped function will be
     assigned to the default group 'Application'. */
  char* func_group;

  /* Flag: Do additional search actual library functions in the LIBC. */
  char  libc;

  /* Flag: Wait for initialization of VampirTrace before generating events by
     the wrapper functions. */
  char  wait_for_init;

  /* Pointer to a function which may be used to initialize the library wrapper
     attributes above. It is called at the first wrapper event, if it is not
     set to NULL. */
  VTLibwrapAttrInitFunc init_func;
};

/* VampirTrace internal functions */

/* initialize library wrapper interface */
void vt_libwrap_init(void);
/* finalize library wrapper interface */
void vt_libwrap_finalize(void);

/* load external LIBC and get its handle */
void* vt_libwrap_get_libc_handle(void);
/* set errno of external LIBC to given value */
void vt_libwrap_set_libc_errno(const int value);
/* get errno of external LIBC */
int vt_libwrap_get_libc_errno(void);

/* create a library wrapper object
   arguments:
     lw       = library wrapper object
                (must be initialized by VT_LIBWRAP_NULL)
     lwattr   = library wrapper attributes */
void VTLibwrap_create(VTLibwrap** lw, VTLibwrapAttr* lwattr);

/* delete a library wrapper object
   arguments:
     lw       = library wrapper object */
void VTLibwrap_delete(VTLibwrap* lw);

/* delete all library wrapper objects */
void VTLibwrap_delete_all(void);

/* initialize a wrapper function
   arguments:
     lw       = library wrapper object
     func     = function name
     file     = function location (file)
     line     = function location (line)
     funcptr  = pointer to real function (return)
     funcid   = function identifier (return) */
void VTLibwrap_func_init(const VTLibwrap* lw, const char* func,
                         const char* file, int line,
                         void** funcptr, int* funcid);

/* trigger a function enter event
   arguments:
     lw       = library wrapper object
     funcid   = function identifier */
void VTLibwrap_func_start(const VTLibwrap* lw, const int funcid);

/* trigger a function leave event
   arguments:
     lw       = library wrapper object
     funcid   = function identifier */
void VTLibwrap_func_end(const VTLibwrap* lw, const int funcid);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _VT_LIBWRAP_H */
