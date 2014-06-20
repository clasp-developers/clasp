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

#include <stdlib.h>
#include <sys/resource.h>

#include "vt_iowrap_helper.h"
#include "vt_defs.h"
#include "vt_error.h"
#include "vt_thrd.h"
#include "vt_trc.h"

/* Maximum number of open files supported by VT if os does not set a limit */
#define VT_MAX_OPEN_FILES 1048576

uint32_t file_group_id_stdio = 0;
uint32_t file_group_id_rest = 0;
int max_open_files = 0;
vampir_file_t *fd_to_vampirid = NULL;
vampir_file_t unknown_fd_vampir_file;


/*
 * Thread safety is achieved through the calling function: vt_iofile_open() is
 * holding VTTHRD_LOCK_IDS when it calls this function
 */
static void store_vampir_file_id(int fd, uint32_t file_id,
    uint32_t file_group_id)
{
  vampir_file_t *file_ptr;

  vt_libassert( (fd < max_open_files) && (fd >= 0) );
  file_ptr = &fd_to_vampirid[fd];
  file_ptr->vampir_file_id = file_id;
  file_ptr->vampir_file_group_id = file_group_id;
  file_ptr->handle = VTTHRD_IO_NEXT_HANDLE(VTTHRD_MY_VTTHRD);
}

vampir_file_t *get_vampir_file(int fd)
{
  vampir_file_t *file_ptr;

  vt_libassert( (fd < max_open_files) && (fd >= 0) );
  file_ptr = &fd_to_vampirid[fd];
  if( file_ptr->vampir_file_id == 0 ) {
    return &unknown_fd_vampir_file;
  }
  else {
    return file_ptr;
  }
}

/** read max open files per process from */
int get_max_open_files(void)
{
  struct rlimit rlp;
  if (getrlimit(RLIMIT_NOFILE, &rlp)!=0)
    vt_error_msg("");
  return (rlp.rlim_max == RLIM_INFINITY) ? VT_MAX_OPEN_FILES :
                                           (int)rlp.rlim_max;
}

/** Create a new OTF file definition entry and associate the id with the fd for
 * later reference. */
void vt_iofile_open(const char* fname, int fd)
{
  uint32_t fid;
  uint32_t gid;

  /* fprintf( stderr, "opening file: %s\n", fname); */
  vt_libassert( (fd < max_open_files) && (fd >= 0) );
  gid=(fd<3) ? file_group_id_stdio : file_group_id_rest;
#if defined (VT_MT) || defined (VT_HYB) || defined (VT_JAVA)
  VTTHRD_LOCK_IDS();
#endif
  fid = vt_def_file(VT_CURRENT_THREAD, fname, gid);
  store_vampir_file_id(fd, fid, gid);
#if defined (VT_MT) || defined (VT_HYB) || defined (VT_JAVA)
  VTTHRD_UNLOCK_IDS();
#endif
}

/** Mark the fd as closed */
void vt_iofile_close(int fd)
{
  vt_libassert( (fd < max_open_files) && (fd >= 0) );
  fd_to_vampirid[fd].vampir_file_id = 0;
}

/* Create a new OTF file definition entry and return its id. 
 * The files created here will belong to the 'normal' file group (not stdio).
 */
uint32_t vt_iofile_id(const char* fname)
{
  uint32_t fid;

#if defined (VT_MT) || defined (VT_HYB) || defined (VT_JAVA)
  VTTHRD_LOCK_IDS();
#endif
  fid = vt_def_file(VT_CURRENT_THREAD, fname, file_group_id_rest);
#if defined (VT_MT) || defined (VT_HYB) || defined (VT_JAVA)
  VTTHRD_UNLOCK_IDS();
#endif
  return fid;
}

void vt_iofile_dupfd(int oldfd, int newfd)
{
  vt_libassert( (newfd < max_open_files) && (newfd >=0) &&
             (oldfd < max_open_files) && (oldfd >=0) );
  fd_to_vampirid[newfd] = fd_to_vampirid[oldfd];
  fd_to_vampirid[newfd].handle = VTTHRD_IO_NEXT_HANDLE(VTTHRD_MY_VTTHRD);
}
