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

#include "installdirs.h"
#include "installdirs_conf.h"
#include "util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef INSIDE_OPENMPI
# define ENV_PREFIX "OPAL_"
#else /* INSIDE_OPENMPI */
# define ENV_PREFIX "VT_"
#endif /* INSIDE_OPENMPI */

/* macro for getting install directory path (dir) either from env. variable or
   from macro in Autoconf generated header file 'installdirs_conf.h'; the
   result will be stored in 'install_dirs' */
#define GETDIR(type, dir)                       \
  if( (dir = install_dirs.type) == NULL ) {     \
    char* tmp = getenv( ENV_PREFIX#type );      \
    if( tmp != NULL && strlen( tmp ) > 0 )      \
      dir = vt_strdup( tmp );                   \
    else                                        \
      dir = vt_installdirs_expand( VT_##type ); \
    install_dirs.type = dir;                    \
  }

/* data structure for holding install directory paths */
static struct install_dirs_struct
{
  char* PREFIX;
  char* EXEC_PREFIX;
  char* BINDIR;
  char* INCLUDEDIR;
  char* LIBDIR;
  char* DATADIR;
  char* DATAROOTDIR;
  char* DOCDIR;
  char* SYSCONFDIR;
} install_dirs;

char* vt_installdirs_get( VTInstallDirT type )
{
  char* ret;

  /* get install directory path by 'type' and store it in 'ret' */

  switch( type )
  {
    case VT_INSTALLDIR_PREFIX:
    {
      GETDIR( PREFIX, ret );
      break;
    }
    case VT_INSTALLDIR_EXEC_PREFIX:
    {
      GETDIR( EXEC_PREFIX, ret );
      break;
    }
    case VT_INSTALLDIR_BINDIR:
    {
      GETDIR( BINDIR, ret );
      break;
    }
    case VT_INSTALLDIR_INCLUDEDIR:
    {
      GETDIR( INCLUDEDIR, ret );
      break;
    }
    case VT_INSTALLDIR_LIBDIR:
    {
      GETDIR( LIBDIR, ret );
      break;
    }
    case VT_INSTALLDIR_DATADIR:
    {
      GETDIR( DATADIR, ret );
      break;
    }
    case VT_INSTALLDIR_DATAROOTDIR:
    {
      GETDIR( DATAROOTDIR, ret );
      break;
    }
    case VT_INSTALLDIR_DOCDIR:
    {
      GETDIR( DOCDIR, ret );
      break;
    }
    case VT_INSTALLDIR_SYSCONFDIR:
    {
      GETDIR( SYSCONFDIR, ret );
      break;
    }
    default:
    {
      /* unknown type - this should never happen */
      return NULL;
    }
  }

  return ret;
}

char* vt_installdirs_expand(const char* input)
{
  char* ret;

  char* start;
  char* end;

  if( input == NULL ) return NULL;

  /* 'input' cannot be modified, so copy it to a new allocated string 'ret' */
  ret = vt_strdup( input );

  /* expand all variables from directory path 'ret' */

  while( ret != NULL && (start = strchr( ret, '$' )) != NULL )
  {
    char* insert;
    char* tmp = ret;

    /* check for variables which can be expanded */

    if( strncmp( start, "${prefix}", 9 ) == 0 )
      insert = vt_installdirs_get( VT_INSTALLDIR_PREFIX );
    else if( strncmp( start, "${exec_prefix}", 14 ) == 0 )
      insert = vt_installdirs_get( VT_INSTALLDIR_EXEC_PREFIX );
    else if( strncmp( start, "${bindir}", 9 ) == 0 )
      insert = vt_installdirs_get( VT_INSTALLDIR_BINDIR );
    else if( strncmp( start, "${includedir}", 13 ) == 0 )
      insert = vt_installdirs_get( VT_INSTALLDIR_INCLUDEDIR );
    else if( strncmp( start, "${libdir}", 9 ) == 0 )
      insert = vt_installdirs_get( VT_INSTALLDIR_LIBDIR );
    else if( strncmp( start, "${datadir}", 10 ) == 0 )
      insert = vt_installdirs_get( VT_INSTALLDIR_DATADIR );
    else if( strncmp( start, "${datarootdir}", 14 ) == 0 )
      insert = vt_installdirs_get( VT_INSTALLDIR_DATAROOTDIR );
    else if( strncmp( start, "${docdir}", 9 ) == 0 )
      insert = vt_installdirs_get( VT_INSTALLDIR_DOCDIR );
    else if( strncmp( start, "${sysconfdir}", 13 ) == 0 )
      insert = vt_installdirs_get( VT_INSTALLDIR_SYSCONFDIR );
    else
      break;

    /* compose output directory path */

    end = strchr( start, '}' );
    *start = '\0';
    vt_asprintf( &ret, "%s%s%s", tmp, insert, end + 1 );

    free( tmp );
  }

  return ret;
}
