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
#include "vt_pform.h"

#include "util/installdirs.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static char* replace_vars(char *v) {
  char* start;
  char* end;
  char* vname;
  char* vval;
  char* res;
  int extra = 0;
  int plen = 0;

  if ((start = strchr(v, '$')) == NULL ) {
    /* no $ in v -> no replacement necessary */
    return strdup(v);
  } else {
    if ( start[1] == '{' ) {
      /* ${....} form */
      extra = 1;
      end = start += 2;
      while ( *end && *end != '}' ) end++;
    } else {
      /* $### form where # is letter, digit, or underscore */
      end = ++start;
      while ( *end && (isalnum(*end) || *end == '_')) end++;
    }
    /* determine name of variable */
    vname = (char*)malloc((end-start)+1);
    strncpy(vname, start, end-start);
    vname[end-start] = '\0';

    /* get its content */
    if ((vval = getenv(vname)) == NULL) vval = "";

    /* put together string with variable replaced by value */
    /* -- allocate enough space and copy stuff before variable part */
    res = (char*)malloc(strlen(v)+strlen(vval)+1);
    plen = (start - v) - 1 - extra;
    if (plen) strncpy(res, v, plen);
    res[plen] = '\0';
    /* -- add variable content */
    strcat(res, vval);
    /* -- add stuff after variable */
    if ( *end ) strcat(res, end + extra);

    free(vname);
    return res;
  }
}

static char* strip_dir(char *path) {
  char* start;
  char* res;

  if ((start = strrchr(path, '/')) == NULL ) {
    /* no / in path -> no removing necessary */
    return path;
  } else {
    if (*(++start) == '\0') {
      /* path has a trailing slash or is "/", return empty string */
      return "";
    } else {
      /* otherwise, strip directory from path and return */
      res = strdup(start);
      return res;
    }
  }
}

static int parse_bool(char *str) {
  static char strbuf[128];
  char* ptr = strbuf;

  strncpy(strbuf, str, sizeof(strbuf)-1);
  while ( *ptr )
    {
      *ptr = tolower(*ptr);
      ++ptr;
    }

  if ( strcmp(strbuf, "yes") == 0  ||
       strcmp(strbuf, "true") == 0 ||
       strcmp(strbuf, "1") == 0)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

static size_t parse_size(char *str) {
  size_t size = 0;

  if (strlen(str) >= 1)
  {
     int multiply = 0;

     switch(str[strlen(str)-1])
     {
       case '0':
       case '1':
       case '2':
       case '3':
       case '4':
       case '5':
       case '6':
       case '7':
       case '8':
       case '9':
         multiply = 1;
         break;
       case 'K':
       case 'k':
         multiply = 1024;
         break;
       case 'M':
       case 'm':
         multiply = 1024*1024;
         break;
       case 'G':
       case 'g':
         multiply = 1024*1024*1024;
         break;
       default:
         break;
     }

     size = atoll(str) * multiply;
  }

  return size;
}

char* vt_env_apppath()
{
  static int read = 1;
  static char* apppath = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_APPPATH");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_APPPATH=%s", tmp);

          apppath = replace_vars(tmp);
        }
      else
        {
          apppath = vt_pform_exec();
        }
    }
  return apppath;
}

char* vt_env_dyn_shlibs()
{
  static int read = 1;
  static char* dyn_shlibs = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_DYN_SHLIBS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_DYN_SHLIBS=%s", tmp);

          dyn_shlibs = replace_vars(tmp);
        }
    }
  return dyn_shlibs;
}

int vt_env_dyn_outer_loops()
{
  static int dyn_outer_loops = -1;
  char* tmp;

  if (dyn_outer_loops == -1)
    {
      tmp = getenv("VT_DYN_OUTER_LOOPS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_DYN_OUTER_LOOPS=%s", tmp);

          dyn_outer_loops = parse_bool(tmp);
        }
      else
        {
          dyn_outer_loops = 0;
        }
    }
  return dyn_outer_loops;
}

int vt_env_dyn_inner_loops()
{
  static int dyn_inner_loops = -1;
  char* tmp;

  if (dyn_inner_loops == -1)
    {
      tmp = getenv("VT_DYN_INNER_LOOPS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_DYN_INNER_LOOPS=%s", tmp);

          dyn_inner_loops = parse_bool(tmp);
        }
      else
        {
          dyn_inner_loops = 0;
        }
    }
  return dyn_inner_loops;
}

int vt_env_dyn_loop_iters()
{
  static int dyn_loop_iters = -1;
  char* tmp;

  if (dyn_loop_iters == -1)
    {
      tmp = getenv("VT_DYN_LOOP_ITERS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_DYN_LOOP_ITERS=%s", tmp);

          dyn_loop_iters = parse_bool(tmp);
        }
      else
        {
          dyn_loop_iters = 0;
        }
    }
  return dyn_loop_iters;
}

int vt_env_dyn_ignore_nodbg()
{
  static int dyn_ignore_nodbg = -1;
  char* tmp;

  if (dyn_ignore_nodbg == -1)
    {
      tmp = getenv("VT_DYN_IGNORE_NODBG");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_DYN_IGNORE_NODBG=%s", tmp);

          dyn_ignore_nodbg = parse_bool(tmp);
        }
      else
        {
          dyn_ignore_nodbg = 0;
        }
    }
  return dyn_ignore_nodbg;
}

int vt_env_dyn_detach()
{
  static int dyn_detach = -1;
  char* tmp;

  if (dyn_detach == -1)
    {
      tmp = getenv("VT_DYN_DETACH");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_DYN_DETACH=%s", tmp);

          dyn_detach = parse_bool(tmp);
        }
      else
        {
          dyn_detach = 1;
        }
    }
  return dyn_detach;
}

char* vt_env_gnu_nm()
{
  static int read = 1;
  static char* gnu_nm = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_GNU_NM");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_GNU_NM=%s", tmp);

          gnu_nm = replace_vars(tmp);
        }
      else
        {
#ifdef DEFAULT_NM
          gnu_nm = DEFAULT_NM;
#endif /* DEFAULT_NM */
        }
    }

  return gnu_nm;
}

char* vt_env_gnu_nmfile()
{
  static int read = 1;
  static char* gnu_nmfile = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_GNU_NMFILE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_GNU_NMFILE=%s", tmp);

          gnu_nmfile = replace_vars(tmp);
        }
    }
  return gnu_nmfile;
}

char* vt_env_gdir()
{
  static char* gdir = NULL;
  char* tmp;

  if (! gdir)
    {
      tmp = getenv("VT_PFORM_GDIR");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_PFORM_GDIR=%s", tmp);

          gdir = replace_vars(tmp);
        }
      else
        {
          gdir = replace_vars(vt_pform_gdir());
        }

#if defined(VT_IOFSL)
      if (vt_env_iofsl_servers())
      {
        char* gdir_abs = realpath(gdir, NULL);
        if (gdir_abs == NULL)
          {
            vt_error_msg("Could not retrieve the absolute path of %s:",
                         gdir, strerror(errno));
          }
        else
          {
            gdir = gdir_abs;
          }
      }
#endif /* VT_IOFSL */
    }
  return gdir;
}

char* vt_env_ldir()
{
  static char* ldir = NULL;
  char* tmp;

  if (! ldir)
    {
      tmp = getenv("VT_PFORM_LDIR");

#if defined(VT_IOFSL)
      if (vt_env_iofsl_servers())
        {
          ldir = vt_env_gdir();

          if (tmp != NULL && strlen(tmp) > 0)
            {
              vt_warning("Setting of VT_PFORM_LDIR isn't allowed in IOFSL "
                         "mode; reset it to VT_PFORM_GDIR (=%s)", ldir);
            }
        }
      else
#endif /* VT_IOFSL */
        {
          if (tmp != NULL && strlen(tmp) > 0)
            {
              vt_cntl_msg(2, "VT_PFORM_LDIR=%s", tmp);

              ldir = replace_vars(tmp);
            }
          else
            {
              ldir = replace_vars(vt_pform_ldir());
            }
        }
    }
  return ldir;
}

int vt_env_gdir_check()
{
  static int gdir_check = -1;
  char* tmp;

  if (gdir_check == -1)
    {
      tmp = getenv("VT_PFORM_GDIR_CHECK");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_PFORM_GDIR_CHECK=%s", tmp);

          gdir_check = parse_bool(tmp);
        }
      else
        {
          gdir_check = 1;
        }
    }
  return gdir_check;
}

int vt_env_ldir_check()
{
  static int ldir_check = -1;
  char* tmp;

  if (ldir_check == -1)
    {
      tmp = getenv("VT_PFORM_LDIR_CHECK");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_PFORM_LDIR_CHECK=%s", tmp);

          ldir_check = parse_bool(tmp);
        }
      else
        {
          ldir_check = 1;
        }
    }
  return ldir_check;
}

char* vt_env_fprefix()
{
  static char* fprefix = NULL;
  char* tmp;

  if (! fprefix)
    {
      tmp = getenv("VT_FILE_PREFIX");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_FILE_PREFIX=%s", tmp);

          fprefix = replace_vars(tmp);
        }
      else
        {
          tmp = vt_env_apppath();
          if (tmp != NULL && strlen(tmp) > 0)
            {
              fprefix = strip_dir(tmp);
              if (strlen(fprefix) >= 4 &&
                  (strcmp(fprefix+(strlen(fprefix)-4), ".out") == 0 ||
                   strcmp(fprefix+(strlen(fprefix)-4), ".exe") == 0))
                {
                  fprefix[strlen(fprefix)-4] = '\0';
                }
            }
          else
            {
              fprefix = "a";
            }
        }
    }
  return fprefix;
}

int vt_env_funique()
{
  static int funique = -2; /* -1 may be used */
  char* tmp;

  if (funique == -2)
    {
      tmp = getenv("VT_FILE_UNIQUE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          char tmpbuf[128];
          char* p;

          vt_cntl_msg(2, "VT_FILE_UNIQUE=%s", tmp);

          p = tmpbuf;
          strncpy(tmpbuf, tmp, 128);
          tmpbuf[127] = '\0';
          while( *p ) { *p = tolower(*p); p++; }

          if (strcmp(tmpbuf, "yes") == 0  ||
              strcmp(tmpbuf, "true") == 0 ||
              strcmp(tmpbuf, "auto") == 0)
            {
              funique = 0;
            }
          else
            {
              funique = atoi(tmp);
              if (funique == 0)
                funique = -1;
              else if (funique < 0)
                vt_error_msg("VT_FILE_UNIQUE not properly set");
            }
        }
      else
        {
          funique = -1;
        }
    }
  return funique;
}

size_t vt_env_bsize()
{
  static size_t buffer_size = 0;
  char* tmp;

  if (buffer_size == 0)
    {
      tmp = getenv("VT_BUFFER_SIZE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_BUFFER_SIZE=%s", tmp);

          buffer_size = parse_size(tmp);
          if (buffer_size <= 0)
            {
              vt_error_msg("VT_BUFFER_SIZE not properly set");
            }
          else if (buffer_size < VT_MIN_BUFSIZE)
            {
              vt_warning("VT_BUFFER_SIZE=%d resized to %d bytes", 
                         buffer_size, VT_MIN_BUFSIZE);
              buffer_size = VT_MIN_BUFSIZE;
            }
          else if (buffer_size > VT_MAX_BUFSIZE)
            {
              vt_warning("VT_BUFFER_SIZE=%d resized to %d bytes",
                         buffer_size, VT_MAX_BUFSIZE);
              buffer_size = VT_MAX_BUFSIZE;
            }
        }
      else
        {
          buffer_size = VT_DEFAULT_BUFSIZE;
        }
    }

  return buffer_size;
}

size_t vt_env_thread_bsize()
{
  static size_t buffer_size = 0;
  char* tmp;

  if (buffer_size == 0)
    {
      tmp = getenv("VT_THREAD_BUFFER_SIZE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_THREAD_BUFFER_SIZE=%s", tmp);

          buffer_size = parse_size(tmp);
          if (buffer_size <= 0)
            {
              vt_error_msg("VT_BUFFER_SIZE not properly set");
            }
          else if (buffer_size < VT_MIN_BUFSIZE)
            {
              vt_warning("VT_BUFFER_SIZE=%d resized to %d bytes", 
                         buffer_size, VT_MIN_BUFSIZE);
              buffer_size = VT_MIN_BUFSIZE;
            }
          else if (buffer_size > VT_MAX_THREAD_BUFSIZE)
            {
              vt_warning("VT_THREAD_BUFFER_SIZE=%d resized to %d bytes",
                         buffer_size, VT_MAX_THREAD_BUFSIZE);
              buffer_size = VT_MAX_THREAD_BUFSIZE;
            }
        }
      else
        {
          buffer_size = 0;
        }
    }

  return buffer_size;
}

int vt_env_pthread_reuse()
{
  static int pthread_reuse = -1;
  char* tmp;

  if (pthread_reuse == -1)
    {
      tmp = getenv("VT_PTHREAD_REUSE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_PTHREAD_REUSE=%s", tmp);

          pthread_reuse = parse_bool(tmp);
        }
      else
        {
          pthread_reuse = 1;
        }
    }
  return pthread_reuse;
}

int vt_env_mode()
{
  static int modeflags = 0;
  char* tmp;

  if (modeflags == 0)
    {
      tmp = getenv("VT_MODE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          char tmpbuf[128];
          char* p;
          char* tk;
          int dc;

          vt_cntl_msg(2, "VT_MODE=%s", tmp);

          p = tmpbuf;
          strncpy(tmpbuf, tmp, 127);
          tmpbuf[127] = '\0';
          while( *p ) { *p = tolower(*p); p++; }

          tk = strtok(tmpbuf, ":");
          dc = 0;
          modeflags = 0;
          do {
            if (dc <= 1 &&
               (strcmp( tk, "trace" ) == 0))
              modeflags |= VT_MODE_TRACE;
            else if(dc <= 1 &&
                    (strcmp( tk, "stat" ) == 0))
              modeflags |= VT_MODE_STAT;
            else
              vt_error_msg("VT_MODE not properly set");
            dc++;
          } while((tk = strtok(0, ":")));
        }
      else
        {
          modeflags = VT_MODE_TRACE;
        }
    }
  return modeflags;
}

int vt_env_stat_intv()
{
  static int stat_intv = -1;
  char* tmp;

  if (stat_intv == -1)
    {
      tmp = getenv("VT_STAT_INTV");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_STAT_INTV=%s", tmp);

          stat_intv = atoi(tmp);
          if (stat_intv < 0)
            vt_error_msg("VT_STAT_INTV not properly set");
        }
      else
        {
          stat_intv = 0;
        }
    }
  return stat_intv;
}

int vt_env_stat_props()
{
  static int propflags = 0;
  char* tmp;

  if (propflags == 0)
    {
      tmp = getenv("VT_STAT_PROPS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          char tmpbuf[128];
          char* p;
          char* tk;
          int dc;

          vt_cntl_msg(2, "VT_STAT_PROPS=%s", tmp);

          p = tmpbuf;
          strncpy(tmpbuf, tmp, 127);
          tmpbuf[127] = '\0';
          while( *p ) { *p = tolower(*p); p++; }

          if (strcmp( tmpbuf, "all" ) == 0)
            {
              propflags = (VT_SUM_PROP_FUNC | VT_SUM_PROP_MSG | VT_SUM_PROP_COLLOP);
            }
          else
            {
              tk = strtok(tmpbuf, ":");
              dc = 0;
              propflags = 0;
              do {
                if (dc <= 2 &&
                    (strcmp( tk, "func" ) == 0))
                  propflags |= VT_SUM_PROP_FUNC;
                else if(dc <= 2 &&
                        (strcmp( tk, "msg" ) == 0))
                  propflags |= VT_SUM_PROP_MSG;
                else if(dc <= 2 &&
                        (strcmp( tk, "collop" ) == 0))
                  propflags |= VT_SUM_PROP_COLLOP;
/*              else if(dc <= 3 &&
                        (strcmp( tk, "fileop" ) == 0))
                        propflags |= VT_SUM_PROP_FILEOP; */
                else
                  vt_error_msg("VT_STAT_PROPS not properly set");
                dc++;
              } while((tk = strtok(0, ":")));
            }
        }
      else
        {
          /* propflags =
               (VT_SUM_PROP_FUNC | VT_SUM_PROP_MSG | VT_SUM_PROP_COLLOP | VT_SUM_PROP_FILEOP); */
          propflags = (VT_SUM_PROP_FUNC | VT_SUM_PROP_MSG | VT_SUM_PROP_COLLOP);
        }
    }
  return propflags;
}

int vt_env_stat_msg_dtls()
{
  static int dtlsflags = 0;
  char* tmp;

  if (dtlsflags == 0)
    {
      tmp = getenv("VT_STAT_MSG_DTLS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          char tmpbuf[128];
          char* p;
          char* tk;
          int dc;

          vt_cntl_msg(2, "VT_STAT_MSG_DTLS=%s", tmp);

          p = tmpbuf;
          strncpy(tmpbuf, tmp, 127);
          tmpbuf[127] = '\0';
          while( *p ) { *p = tolower(*p); p++; }

          tk = strtok(tmpbuf, ":");
          dc = 0;
          dtlsflags = 0;
          do {
            if (dc <= 2 &&
               (strcmp( tk, "peer" ) == 0))
              dtlsflags |= VT_SUM_MSG_DTL_PEER;
            else if(dc <= 2 &&
                    (strcmp( tk, "comm" ) == 0))
              dtlsflags |= VT_SUM_MSG_DTL_COMM;
            else if(dc <= 2 &&
                    (strcmp( tk, "tag" ) == 0))
              dtlsflags |= VT_SUM_MSG_DTL_TAG;
            else
              vt_error_msg("VT_STAT_MSG_DTLS not properly set");
            dc++;
          } while((tk = strtok(0, ":")));
        }
      else
        {
          dtlsflags = VT_SUM_MSG_DTL_PEER;
        }
    }
  return dtlsflags;
}

int vt_env_stat_collop_dtls()
{
  static int dtlsflags = 0;
  char* tmp;

  if (dtlsflags == 0)
    {
      tmp = getenv("VT_STAT_COLLOP_DTLS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          char tmpbuf[128];
          char* p;
          char* tk;
          int dc;

          vt_cntl_msg(2, "VT_STAT_COLLOP_DTLS=%s", tmp);

          p = tmpbuf;
          strncpy(tmpbuf, tmp, 127);
          tmpbuf[127] = '\0';
          while( *p ) { *p = tolower(*p); p++; }

          tk = strtok(tmpbuf, ":");
          dc = 0;
          dtlsflags = 0;
          do {
            if (dc <= 1 &&
               (strcmp( tk, "comm" ) == 0))
              dtlsflags |= VT_SUM_COLLOP_DTL_COMM;
            else if(dc <= 1 &&
                    (strcmp( tk, "op" ) == 0))
              dtlsflags |= VT_SUM_COLLOP_DTL_OP;
            else
              vt_error_msg("VT_STAT_COLLOP_DTLS not properly set");
            dc++;
          } while((tk = strtok(0, ":")));
        }
      else
        {
          dtlsflags = VT_SUM_COLLOP_DTL_OP;
        }
    }
  return dtlsflags;
}

int vt_env_snapshots()
{
  static int snapshots = -1;
  char* tmp;

  if (snapshots == -1)
    {
      tmp = getenv("VT_SNAPSHOTS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_SNAPSHOTS=%s", tmp);

          snapshots = parse_bool(tmp);
        }
      else
        {
          snapshots = 1;
        }
    }
  return snapshots;
}

int vt_env_max_snapshots()
{
  static int max_snapshots = -1;
  char* tmp;

  if (max_snapshots == -1)
    {
      tmp = getenv("VT_MAX_SNAPSHOTS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_MAX_SNAPSHOTS=%s", tmp);

          max_snapshots = atoi(tmp);
          if (max_snapshots < 1)
            vt_error_msg("VT_MAX_SNAPSHOTS not properly set");
        }
      else
        {
          max_snapshots = 1024;
        }
    }
  return max_snapshots;
}

int vt_env_verbose()
{
  static int verbose = -1;
  char* tmp;

  if (verbose == -1)
    {
      tmp = getenv("VT_VERBOSE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          verbose = atoi(tmp);
          if (verbose < 0)
            verbose = 0;
          else if (verbose > VT_MAX_VERBOSE_LEVEL)
            verbose = VT_MAX_VERBOSE_LEVEL;

          vt_cntl_msg(2, "VT_VERBOSE=%s", tmp);
        }
      else
        {
          verbose = 1;
        }
    }
  return verbose;
}

int vt_env_do_unify()
{
  static int do_unify = -1;
  char* tmp;

  if (do_unify == -1)
    {
      tmp = getenv("VT_UNIFY");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_UNIFY=%s", tmp);

          do_unify = parse_bool(tmp);
        }
      else
        {
          do_unify = 1;
        }
    }
  return do_unify;
}

int vt_env_do_clean()
{
  static int do_clean = -1;
  char* tmp;

  if (do_clean == -1)
    {
      tmp = getenv("VT_CLEAN");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_CLEAN=%s", tmp);

          do_clean = parse_bool(tmp);
        }
      else
        {
          do_clean = 1;
        }
    }
  return do_clean;
}

int vt_env_cpuidtrace()
{
  static int cpuidtrace = -1;
  char* tmp;

  if (cpuidtrace == -1)
    {
      tmp = getenv("VT_CPUIDTRACE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_CPUIDTRACE=%s", tmp);

          cpuidtrace = parse_bool(tmp);
        }
      else
        {
          cpuidtrace = 0;
        }
    }
  return cpuidtrace;
}

int vt_env_iotrace()
{
  static int iotrace = -1;
  char* tmp;

  if (iotrace == -1)
    {
      tmp = getenv("VT_IOTRACE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_IOTRACE=%s", tmp);

          iotrace = parse_bool(tmp);
        }
      else
        {
		  /* if iotrace is not enabled, iotrace can also be enabled through
			 VT_IOTRACE_EXTENDED, so this is tested as well */
		  iotrace = vt_env_iotrace_extended();
        }
    }
  return iotrace;
}

int vt_env_iotrace_extended()
{
  static int iotrace_extended = -1;
  char* tmp;

  if (iotrace_extended == -1)
    {
      tmp = getenv("VT_IOTRACE_EXTENDED");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_IOTRACE_EXTENDED=%s", tmp);

          iotrace_extended = parse_bool(tmp);
        }
      else
        {
          iotrace_extended = 0;
        }
    }
  return iotrace_extended;
}

char* vt_env_iolibpathname()
{
  static char* pathname = NULL;
  char* tmp;

  if (! pathname)
    {
      tmp = getenv("VT_IOLIB_PATHNAME");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_IOLIB_PATHNAME=%s", tmp);

          pathname = replace_vars(tmp);
        }
      else
        {
          pathname = NULL;
        }
    }
  return pathname;
}

int vt_env_exectrace()
{
  static int exectrace = -1;
  char* tmp;

  if (exectrace == -1)
    {
      tmp = getenv("VT_EXECTRACE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_EXECTRACE=%s", tmp);

          exectrace = parse_bool(tmp);
        }
      else
        {
          tmp = getenv("VT_LIBCTRACE");
          if (tmp != NULL && strlen(tmp) > 0)
            {
              exectrace = parse_bool(tmp);

              vt_warning("VT_LIBCTRACE is deprecated, use VT_EXECTRACE instead!");
            }
          else
            {
              exectrace = 1;
            }
        }
    }
  return exectrace;
}

int vt_env_memtrace()
{
  static int memtrace = -1;
  char* tmp;

  if (memtrace == -1)
    {
      tmp = getenv("VT_MEMTRACE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_MEMTRACE=%s", tmp);

          memtrace = parse_bool(tmp);
        }
      else
        {
          memtrace = 0;
        }
    }
  return memtrace;
}

int vt_env_memtrace_marker()
{
  static int memtrace_marker = -1;
  char* tmp;

  if (memtrace_marker == -1)
    {
      tmp = getenv("VT_MEMTRACE_MARKER");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_MEMTRACE_MARKER=%s", tmp);

          memtrace_marker = parse_bool(tmp);
        }
      else
        {
          memtrace_marker = 0;
        }
    }
  return memtrace_marker;
}

int vt_env_omptrace()
{
  static int omptrace = -1;
  char* tmp;

  if (omptrace == -1)
    {
      tmp = getenv("VT_OMPTRACE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_OMPTRACE=%s", tmp);

          omptrace = parse_bool(tmp);
        }
      else
        {
          omptrace = 1;
        }
    }
  return omptrace;
}

int vt_env_mpitrace()
{
  static int mpitrace = -1;
  char* tmp;

  if (mpitrace == -1)
    {
      tmp = getenv("VT_MPITRACE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_MPITRACE=%s", tmp);

          mpitrace = parse_bool(tmp);
        }
      else
        {
          mpitrace = 1;
        }
    }
  return mpitrace;
}

int vt_env_mpi_ignore_filter()
{
  static int mpi_ignore_filter = -1;
  char* tmp;

  if (mpi_ignore_filter == -1)
    {
      tmp = getenv("VT_MPI_IGNORE_FILTER");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_MPI_IGNORE_FILTER=%s", tmp);

          mpi_ignore_filter = parse_bool(tmp);
        }
      else
        {
          mpi_ignore_filter = 0;
        }
    }
  return mpi_ignore_filter;
}

int vt_env_mpicheck()
{
  static int mpicheck = -1;
  char* tmp;

  if (mpicheck == -1)
    {
      tmp = getenv("VT_MPICHECK");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_MPICHECK=%s", tmp);

          mpicheck = parse_bool(tmp);
        }
      else
        {
          mpicheck = 0;
        }
    }
  return mpicheck;
}

int vt_env_mpicheck_errexit()
{
  static int mpicheck_errexit = -1;
  char* tmp;

  if (mpicheck_errexit == -1)
    {
      tmp = getenv("VT_MPICHECK_ERREXIT");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_MPICHECK_ERREXIT=%s", tmp);

          mpicheck_errexit = parse_bool(tmp);
        }
      else
        {
          mpicheck_errexit = 0;
        }
    }
  return mpicheck_errexit;
}

char* vt_env_rusage()
{
  static int read = 1;
  static char* rusage = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_RUSAGE");
      if (tmp && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_RUSAGE=%s", tmp);

          rusage = tmp;
        }
    }
  return rusage;
}

int vt_env_rusage_intv()
{
  static int rusage_intv = -1;
  char* tmp;

  if (rusage_intv == -1)
    {
      tmp = getenv("VT_RUSAGE_INTV");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_RUSAGE_INTV=%s", tmp);

          rusage_intv = atoi(tmp);
          if (rusage_intv < 0)
            vt_error_msg("VT_RUSAGE_INTV not properly set");
        }
      else
        {
          rusage_intv = 100;
        }
    }
  return rusage_intv;
}

char* vt_env_metrics()
{
  static int read = 1;
  static char* metrics = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_METRICS");
      if (tmp && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_METRICS=%s", tmp);

          metrics = tmp;
        }
    }
  return metrics;
}

char* vt_env_metrics_sep()
{
  static char* metrics_sep = NULL;
  char* tmp;

  if (!metrics_sep)
    {
      tmp = getenv("VT_METRICS_SEP");
      if (tmp && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_METRICS_SEP=%s", tmp);

          metrics_sep = tmp;
        }
      else
        {
          metrics_sep = ":";
        }
    }
  return metrics_sep;
}

/* The file with the metrics specifications can be defined with the
VT_METRICS_SPEC environment variable, otherwise it is looked for in
the current directory and the VampirTrace installation DATADIR. */

#define METRICS_SPEC "METRICS.SPEC"

char* vt_env_metrics_spec()
{
  char  msg[128];
  char* spec = getenv("VT_METRICS_SPEC");
  int   len;

  if ( spec != NULL && strlen(spec) > 0 ) { /* use specified file */
    snprintf(msg, sizeof(msg)-1, "VT_METRICS_SPEC=%s", spec);
  } else if (access(METRICS_SPEC, R_OK) == 0) {
    /* use file in current directory */
    len = strlen(METRICS_SPEC)+3;
    spec = (char*)calloc(len, sizeof(char));
    snprintf(spec, len-1, "./%s", METRICS_SPEC);
    snprintf(msg, sizeof(msg)-1, "[CURDIR] VT_METRICS_SPEC=%s", spec);
  } else {
    char* datadir = vt_installdirs_get(VT_INSTALLDIR_DATADIR);
    /* default to installation file */
    len = strlen(datadir)+strlen(METRICS_SPEC)+3;
    spec = (char*)calloc(len, sizeof(char));
    snprintf(spec, len-1, "%s/%s", datadir, METRICS_SPEC);
    snprintf(msg, sizeof(msg)-1, "[DATADIR] VT_METRICS_SPEC=%s", spec);
  }
  vt_cntl_msg(2, msg);
  return spec;
}

int vt_env_sync_flush()
{
  static int sync_flush = -1;
  char* tmp;

  if (sync_flush == -1)
    {
      tmp = getenv("VT_SYNC_FLUSH");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_SYNC_FLUSH=%s", tmp);

          sync_flush = parse_bool(tmp);
        }
      else
        {
          sync_flush = 0;
        }
    }
  return sync_flush;
}

int vt_env_sync_flush_skip(void)
{
  static int skip = -1;
  if (skip == -1)
    {
      char* tmp = getenv("VT_SYNC_FLUSH_SKIP");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_SYNC_FLUSH_SKIP=%s", tmp);

          skip = atoi(tmp);
          if (skip < 0) skip = 0;
        }
      else
        {
          skip = 0;
        }
    }
  return skip;
}

int vt_env_sync_flush_level()
{
  static int sync_flush_level = -1;
  char* tmp;

  if (sync_flush_level == -1)
    {
      tmp = getenv("VT_SYNC_FLUSH_LEVEL");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_SYNC_FLUSH_LEVEL=%s", tmp);

          sync_flush_level = atoi(tmp);
          if (sync_flush_level < 0 || sync_flush_level > 100)
            vt_error_msg("VT_SYNC_FLUSH_LEVEL not properly set");
        }
      else
        {
           sync_flush_level = 80;
        }
    }
  return sync_flush_level;
}

int vt_env_onoff_check_stack_balance()
{
  static int check_stack_balance = -1;

  if (check_stack_balance == -1)
    {
      char* tmp = getenv("VT_ONOFF_CHECK_STACK_BALANCE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_ONOFF_CHECK_STACK_BALANCE=%s", tmp);

          check_stack_balance = parse_bool(tmp);
        }
      else
        {
          check_stack_balance = 1;
        }
    }
  return check_stack_balance;
}

int vt_env_max_stack_depth()
{
  static int max_stack_depth = -1;
  char* tmp;

  if (max_stack_depth == -1)
    {
      tmp = getenv("VT_MAX_STACK_DEPTH");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_MAX_STACK_DEPTH=%s", tmp);

          max_stack_depth = atoi(tmp);
          if (max_stack_depth < 0)
            vt_error_msg("VT_MAX_STACK_DEPTH not properly set");
        }
      else
        {
          max_stack_depth = 0;
        }
    }
  return max_stack_depth;
}

int vt_env_max_flushes()
{
  static int max_flushes = -1;
  char* tmp;

  if (max_flushes == -1)
    {
      tmp = getenv("VT_MAX_FLUSHES");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_MAX_FLUSHES=%s", tmp);

          max_flushes = atoi(tmp);
          if (max_flushes < 0)
            vt_error_msg("VT_MAX_FLUSHES not properly set");
        }
      else
        {
          max_flushes = 1;
        }
    }
  return max_flushes;
}

int vt_env_max_threads()
{
  static int max_threads = -1;
  char* tmp;

  if (max_threads == -1)
    {
      tmp = getenv("VT_MAX_THREADS");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_MAX_THREADS=%s", tmp);

          max_threads = atoi(tmp);
          if (max_threads < 1 || max_threads > VT_MAX_THREADS)
            vt_error_msg("VT_MAX_THREADS not properly set");
        }
      else
        {
          max_threads = VT_MAX_THREADS;
        }
    }
  return max_threads;
}

int vt_env_compression()
{
#if defined(HAVE_ZLIB) && HAVE_ZLIB
  static int compression = -1;
  char* tmp;

  if (compression == -1)
    {
      tmp = getenv("VT_COMPRESSION");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_COMPRESSION=%s", tmp);

          compression = parse_bool(tmp);
        }
      else
        {
          compression = 1;
        }
    }
  return compression;
#else /* HAVE_ZLIB */
  return 0;
#endif /* HAVE_ZLIB */
}

size_t vt_env_compression_bsize(void)
{
  static size_t bsize = 0;
  if (bsize == 0)
    {
      char* tmp = getenv("VT_COMPRESSION_BUFFER_SIZE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_COMPRESSION_BUFFER_SIZE=%s", tmp);

          bsize = parse_size(tmp);
        }
    }
  return bsize;
}

size_t vt_env_otf_bsize(void)
{
  static size_t bsize = 0;
  if (bsize == 0)
    {
      char* tmp = getenv("VT_OTF_BUFFER_SIZE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_OTF_BUFFER_SIZE=%s", tmp);

          bsize = parse_size(tmp);
        }
    }
  return bsize;
}

int vt_env_java_native()
{
  static int native = -1;
  char* tmp;

  if (native == -1)
    {
      tmp = getenv("VT_JAVA_NATIVE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_JAVA_NATIVE=%s", tmp);

          native = parse_bool(tmp);
        }
      else
        {
          native = 0;
        }
    }
  return native;
}

int vt_env_java_synthetic()
{
  static int synthetic = -1;
  char* tmp;

  if (synthetic == -1)
    {
      tmp = getenv("VT_JAVA_SYNTHETIC");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_JAVA_SYNTHETIC=%s", tmp);

          synthetic = parse_bool(tmp);
        }
      else
        {
          synthetic = 0;
        }
    }
  return synthetic;
}

int vt_env_java_group_classes()
{
  static int group_classes = -1;
  char* tmp;

  if (group_classes == -1)
    {
      tmp = getenv("VT_JAVA_GROUP_CLASSES");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_JAVA_GROUP_CLASSES=%s", tmp);

          group_classes = parse_bool(tmp);
        }
      else
        {
          group_classes = 1;
        }
    }
  return group_classes;
}

char* vt_env_java_filter_spec()
{
  static int read = 1;
  static char* spec = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_JAVA_FILTER_SPEC");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_JAVA_FILTER_SPEC=%s", tmp);

          spec = replace_vars(tmp);
        }
    }
  return spec;
}

char* vt_env_filter_spec()
{
  static int read = 1;
  static char* spec = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_FILTER_SPEC");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_FILTER_SPEC=%s", tmp);

          spec = replace_vars(tmp);
        }
    }
  return spec;
}

char* vt_env_groups_spec()
{
  static int read = 1;
  static char* spec = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_GROUPS_SPEC");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_GROUPS_SPEC=%s", tmp);

          spec = replace_vars(tmp);
        }
    }
  return spec;
}

int vt_env_etimesync()
{
  static int etimesync = -1;
  char* tmp;

  if (etimesync == -1)
    {
      tmp = getenv("VT_ETIMESYNC");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_ETIMESYNC=%s", tmp);

          etimesync = parse_bool(tmp);
        }
      else
        {
          etimesync = 0;
        }
    }
  return etimesync;
}

int vt_env_etimesync_intv()
{
  static int etimesync_intv = -1;
  char* tmp;

  if (etimesync_intv == -1)
    {
      tmp = getenv("VT_ETIMESYNC_INTV");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_ETIMESYNC_INTV=%s", tmp);

          etimesync_intv = atoi(tmp);
          if (etimesync_intv < 0)
            vt_error_msg("VT_ETIMESYNC_INTV not properly set");
        }
      else
        {
          etimesync_intv = 120;
        }
    }
  return etimesync_intv;
}

void vt_env_cudatrace()
{
  char* tmp = getenv("VT_CUDATRACE");
  if (tmp != NULL && strlen(tmp) > 0)
    {
      /* split error message in three parts due to C89 limitations */ 
      char* error_msg[3] = {
        "VT_CUDATRACE has been replaced by VT_GPUTRACE!\n"
        "Usage: export VT_GPUTRACE=option1,option2,option2,...\n"
        "The following CUDA measurement options are available:\n"
        " cuda      : enable CUDA (needed to use CUDA runtime API wrapper)\n"
        " cupti     : use the CUPTI interface instead of the library wrapper\n",
        " runtime   : CUDA runtime API\n"
        " driver    : CUDA driver API\n"
        " kernel    : CUDA kernels\n"
        " concurrent: enable concurrent kernel tracing at initialization time\n"
        " idle      : GPU compute idle time\n"
        " memcpy    : CUDA memory copies\n"
        " memusage  : CUDA memory allocation\n"
        " debug     : CUDA tracing debug mode\n"
        " error     : CUDA errors will exit the program\n"
        " yes|default: same as 'cuda,runtime,kernel,memcpy'\n"
        " no: disable CUDA measurement\n",
        
        "VT_CUDATRACE_CUPTI, VT_CUDATRACE_MEMCPY, VT_GPUTRACE_IDLE, "
        "VT_GPUTRACE_ERROR have been replaced by VT_GPUTRACE as well!\n"
        "Read the user manual for further information!\n" };

      vt_error_msg("%s%s%s", error_msg[0], error_msg[1], error_msg[2]);
    }
}

size_t vt_env_cudatrace_bsize()
{
  static size_t limit = 0;

  if (limit == 0)
    {
      char* tmp = getenv("VT_CUDATRACE_BUFFER_SIZE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_CUDATRACE_BUFFER_SIZE=%s", tmp);

          limit = parse_size(tmp);
        }
    }
  return limit;
}

char* vt_env_cupti_events()
{
  static int read = 1;
  static char* events = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_CUPTI_METRICS");
      if (tmp && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_CUPTI_METRICS=%s", tmp);

          events = tmp;
        }
    }
  return events;
}

int vt_env_cupti_sampling()
{
  static int cuptisampling = -1;

  if (cuptisampling == -1)
    {
      char* tmp = getenv("VT_CUPTI_EVENTS_SAMPLING");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_CUPTI_EVENTS_SAMPLING=%s", tmp);

          cuptisampling = parse_bool(tmp);
        }
      else
        {
          cuptisampling = 0;
        }
    }
  return cuptisampling;
}

char* vt_env_gputrace()
{
  static int read = 1;
  static char* args = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_GPUTRACE");
      if (tmp && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_GPUTRACE=%s", tmp);

          args = tmp;
        }
    }
  return args;
}

int vt_env_gputrace_kernel()
{
  static int cudakernel = -1;

  if (cudakernel == -1)
    {
      char* tmp = getenv("VT_GPUTRACE_KERNEL");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_GPUTRACE_KERNEL=%s", tmp);

          cudakernel = atoi(tmp);
          /* perhaps user wrote 'yes' or 'true' */
          if(cudakernel == 0 && parse_bool(tmp) == 1) cudakernel = 1;
          
          if(cudakernel == 1)
            vt_warning("VT_GPUTRACE_KERNEL is deprecated, "
                      "use option 'kernel' with VT_GPUTRACE instead!");
        }
      else
        {
          cudakernel = 1;
        }
    }
  return cudakernel;
}

int vt_env_gputrace_memusage()
{
  static int gpumem = -1;
  
  if (gpumem == -1)
    {
      char* tmp = getenv("VT_GPUTRACE_MEMUSAGE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_GPUTRACE_MEMUSAGE=%s", tmp);

          gpumem = atoi(tmp);
          /* if user wrote 'yes' or 'true' */
          if(gpumem == 0 && parse_bool(tmp) == 1) gpumem = 1;
        }
      else
        {
          gpumem = 0;
        }
      
      if (gpumem > 0)
        vt_warning("VT_GPUTRACE_MEMUSAGE is deprecated, "
                  "use option 'memusage' with VT_GPUTRACE instead!");
    }
  return gpumem;
}

int vt_env_gputrace_sync()
{
  static int sync = -1;

  if (sync == -1)
    {
      char* tmp = getenv("VT_GPUTRACE_SYNC");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_GPUTRACE_SYNC=%s", tmp);

          sync = atoi(tmp);
          /* perhaps user wrote 'yes' or 'true' */
          if(sync == 0 && parse_bool(tmp) == 1) sync = 3;
        }
      else
        {
          sync = 3;
        }
    }
  return sync;
}

char* vt_env_iofsl_servers()
{
  static int read = 1;
  static char* iofsl_servers = NULL;
  char* tmp;

  if (read)
    {
      read = 0;
      tmp = getenv("VT_IOFSL_SERVERS");
      if (tmp && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_IOFSL_SERVERS=%s", tmp);

          iofsl_servers = tmp;
        }
    }
  return iofsl_servers;
}

int vt_env_iofsl_mode()
{
  static int mode = -1;
  char* tmp;

  if (mode == -1)
    {
      tmp = getenv("VT_IOFSL_MODE");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          char tmpbuf[128];
          char* p;

          vt_cntl_msg(2, "VT_IOFSL_MODE=%s", tmp);

          p = tmpbuf;
          strncpy(tmpbuf, tmp, 127);
          tmpbuf[127] = '\0';
          while( *p ) { *p = tolower(*p); p++; }

          if (strcmp(tmpbuf, "multifile") == 0)
            mode = VT_IOFSL_MODE_MULTIFILE;
          else if (strcmp(tmpbuf, "multifile_split") == 0)
            mode = VT_IOFSL_MODE_MULTIFILE_SPLIT;
          else
            vt_error_msg("VT_IOFSL_MODE not properly set");
        }
      else
        {
          mode = VT_IOFSL_MODE_MULTIFILE_SPLIT;
        }
    }
  return mode;
}

int vt_env_iofsl_async_io()
{
  static int async_io = -1;

  if (async_io == -1)
    {
      char* tmp = getenv("VT_IOFSL_ASYNC_IO");
      if (tmp != NULL && strlen(tmp) > 0)
        {
          vt_cntl_msg(2, "VT_IOFSL_ASYNC_IO=%s", tmp);

          async_io = parse_bool(tmp);
        }
      else
        {
          async_io = 0;
        }
    }
  return async_io;
}
