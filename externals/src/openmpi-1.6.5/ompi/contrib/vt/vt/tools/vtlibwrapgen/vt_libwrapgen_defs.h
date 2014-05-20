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
 *
 * tools/vtlibwrapgen/vt_libwrapgen_defs.h.  Generated from vt_libwrapgen_defs.h.in by configure.
 **/

#ifndef _VT_LIBWRAPGEN_DEFS_H_
#define _VT_LIBWRAPGEN_DEFS_H_

#define VT_LIBWRAPGEN_VERSION                   "5.14.4openmpi"
#define VT_LIBWRAPGEN_DEFAULT_OUTPUT_SRC_FILE   "wrap.c"
#define VT_LIBWRAPGEN_DEFAULT_OUTPUT_LIB_PREFIX "libwrap"
#define VT_LIBWRAPGEN_DEFAULT_LIBTOOL           "${datadir}/libtool"
#define VT_LIBWRAPGEN_DEFAULT_CC                "/usr/bin/clang"
#define VT_LIBWRAPGEN_DEFAULT_CPP               "/usr/bin/clang -E"
#define VT_LIBWRAPGEN_DEFAULT_CFLAGS            "-DNDEBUG -g -O2 -finline-functions -fno-strict-aliasing "
#define VT_LIBWRAPGEN_DEFAULT_SYSHEADER_PREFIX  "/usr/include/"

#endif // _VT_LIBWRAPGEN_H_
