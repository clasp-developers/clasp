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

#include "config.h"

#include "vt_env.h"
#include "vt_error.h"
#include "vt_fork.h"
#include "vt_inttypes.h"
#include "vt_iowrap.h"
#include "vt_execwrap.h"
#include "vt_pform.h"
#include "vt_trc.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

static char     trcid_filename[VT_PATH_MAX] = "";
static pid_t*   childv;
static uint32_t nchilds;
static uint8_t  fork_performed = 0;

static void childv_add(pid_t pid);
static int  get_new_trcid(void);

static void childv_add(pid_t pid)
{
  childv = (pid_t*)realloc(childv, (nchilds+1) * sizeof(pid_t));
  if (childv == NULL )
    vt_error();

  childv[nchilds++] = pid;
}

static int get_new_trcid()
{
  int new_trcid;
  int fd;
  int8_t tmp_len;
  struct flock fl;
  char tmp[10] = "";
  uint8_t do_unlock = 1;

  vt_libassert(trcid_filename[0] != '\0');

  VT_SUSPEND_IO_TRACING(VT_CURRENT_THREAD);

  /* open/create temp. id file */
  if ( (fd = open(trcid_filename,
		  (O_RDWR | O_CREAT),
		  (S_IRUSR | S_IWUSR))) == -1 )
    vt_error_msg("Cannot open file %s: %s", trcid_filename, strerror(errno));

  /* lock temp. id file */
  fl.l_type = F_WRLCK; fl.l_whence = SEEK_SET; fl.l_start = 0; fl.l_len = 0;
  if (fcntl(fd, F_SETLKW, &fl) == -1)
  {
    do_unlock = 0;
    vt_warning("Cannot lock file %s: %s", trcid_filename, strerror(errno));
  }

  /* read current trace id */
  if ( read(fd, tmp, 9) == -1 )
    vt_error_msg("Cannot read file %s: %s", trcid_filename, strerror(errno));
  tmp[9] = '\0';

  if ( tmp[0] == '\0' )
    new_trcid = 1;             /* set trace id to 1, if file is empty */
  else
    new_trcid = atoi(tmp) + 1; /* increment trace id */

  /* write new trace id */
  lseek(fd, 0, SEEK_SET);
  snprintf(tmp, sizeof(tmp)-1, "%i\n", new_trcid);
  tmp_len = strlen( tmp );
  if( tmp_len > write( fd, tmp, tmp_len ) ){
    vt_error_msg( "Failed to write to file %s: %s", trcid_filename,strerror(errno) );
  }

  /* unlock temp. id file */
  if ( do_unlock )
  {
    fl.l_type = F_UNLCK;
    if ( fcntl(fd, F_SETLK, &fl) == -1 )
      vt_error_msg("Cannot unlock file %s: %s", trcid_filename, strerror(errno));
  }

  /* close temp. id file */
  close(fd);

  vt_cntl_msg(2, "Updated trace-id in %s to %i", trcid_filename, new_trcid);

  VT_RESUME_IO_TRACING(VT_CURRENT_THREAD);

  return new_trcid;
}

void vt_fork_init()
{
  /* create temp. id filename, if necessary */
  if ( trcid_filename[0] == '\0' )
  {
    snprintf(trcid_filename, sizeof(trcid_filename)-1, "%s/%s.%lx.%u.trcid.tmp",
	           vt_env_ldir(), vt_env_fprefix(), vt_pform_node_id(), getpid());
  }
}

void vt_fork_finalize()
{
  if ( nchilds > 0 )
    free(childv);
  nchilds = 0;
}

void vt_fork(pid_t pid)
{
  vt_libassert(pid != -1);

  fork_performed = 1;

  /* child process ... */
  if ( pid == 0 )
  {
    vt_my_ptrace = vt_my_trace;
    vt_my_trace = get_new_trcid();

    vt_error_pid(vt_my_trace);

    /* reset VampirTrace for new child process */
    vt_reset();
  }
  /* parent process ... */
  else
  {
    vt_error_pid(vt_my_trace);

    /* add new pid to child vector */
    childv_add(pid);
  }
}

void vt_fork_waitchilds()
{
  uint32_t i;
  int status;

  if ( nchilds == 0 ) return;

  VT_SUSPEND_EXEC_TRACING(VT_CURRENT_THREAD);

  /* wait until all child processes are terminated */
  for( i = 0; i < nchilds; i++ )
  {
    vt_cntl_msg(2, "Waiting until child process %i terminated",
		(int)childv[i]);

    waitpid(childv[i], &status, 0);

    vt_cntl_msg(2, "Child process %i terminated, leaving %u",
		(int)childv[i], nchilds-i-1);
  }

  VT_RESUME_EXEC_TRACING(VT_CURRENT_THREAD);
}

uint32_t vt_fork_get_num_childs()
{
  return nchilds;
}

uint32_t vt_fork_get_num_childs_tot()
{
  uint32_t nchilds_tot;

  /* any fork performed? (trace-id file exists?) */
  if ( fork_performed )
  {
    int fd;
    char tmp[16] = "";

    vt_libassert(trcid_filename[0] != '\0');

    VT_SUSPEND_IO_TRACING(VT_CURRENT_THREAD);

    /* open temp. id file for reading */
    if ( (fd = open(trcid_filename, O_RDONLY)) == -1 )
      vt_error_msg("Cannot open file %s: %s", trcid_filename, strerror(errno));

    /* read current trace id */
    if ( read(fd, tmp, 16) == -1 )
      vt_error_msg("Cannot read file %s: %s", trcid_filename, strerror(errno));

    vt_libassert(tmp[0] != '\0');
    nchilds_tot = atoi(tmp);
    vt_libassert(nchilds_tot > 0);

    /* close temp. id file */
    close(fd);

    VT_RESUME_IO_TRACING(VT_CURRENT_THREAD);
  }
  else
  {
    nchilds_tot = 0;
  }
  
  return nchilds_tot;
}

char* vt_fork_get_trcid_filename()
{
  char* filename;

  vt_libassert(trcid_filename[0] != '\0');

  filename = strdup(trcid_filename);
  if ( filename == NULL )
    vt_error();

  return filename;
}
