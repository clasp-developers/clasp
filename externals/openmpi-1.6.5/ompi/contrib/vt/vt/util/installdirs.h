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

#ifndef _INSTALLDIRS_H
#define _INSTALLDIRS_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

typedef enum {
  VT_INSTALLDIR_PREFIX,
  VT_INSTALLDIR_EXEC_PREFIX,
  VT_INSTALLDIR_BINDIR,
  VT_INSTALLDIR_INCLUDEDIR,
  VT_INSTALLDIR_LIBDIR,
  VT_INSTALLDIR_DATADIR,
  VT_INSTALLDIR_DATAROOTDIR,
  VT_INSTALLDIR_DOCDIR,
  VT_INSTALLDIR_SYSCONFDIR
} VTInstallDirT;

EXTERN char* vt_installdirs_get( VTInstallDirT type );

EXTERN char* vt_installdirs_expand( const char* input );

#endif /* _INSTALLDIRS_H */
