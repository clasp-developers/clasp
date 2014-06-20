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

#define _GNU_SOURCE

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "vt_comp.h"
#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_iowrap.h"
#include "vt_mallocwrap.h"
#include "vt_pform.h"
#include "vt_trc.h"
#include "vt_thrd.h"

#if (defined(HAVE_DL) && HAVE_DL) && (defined(HAVE_DECL_RTLD_DEFAULT) && HAVE_DECL_RTLD_DEFAULT)
# include <dlfcn.h>
# define GET_SO_FUNC_ADDR(func) \
  GET_IA64_FUNC_ADDR(dlsym(RTLD_DEFAULT, (func)))
#else /* HAVE_DL && HAVE_DECL_RTLD_DEFAULT */
# define GET_SO_FUNC_ADDR(func) 0
#endif /* HAVE_DL && HAVE_DECL_RTLD_DEFAULT */

#ifdef __ia64__
# define GET_IA64_FUNC_ADDR(addr) (long)((addr) ? *(void**)(addr) : (addr))
#else /* __ia64__ */
# define GET_IA64_FUNC_ADDR(addr) (long)(addr)
#endif /* __ia64__ */

#define GET_THREAD_ID(tid) \
  VT_CHECK_THREAD;         \
  (tid) = VT_MY_THREAD

#ifdef VT_COMPINST_CRAYCCE
# define __cyg_profile_func_enter __pat_tp_func_entry
# define __cyg_profile_func_exit  __pat_tp_func_return
#endif /* VT_COMPINST_CRAYCCE */

#define NM_LINE_BLK_LEN 1024
#define NM_LINE_MAX_LEN 16384

static int gnu_init = 1;       /* is initialization needed? */

/*
 *-----------------------------------------------------------------------------
 * Simple hash table to map function addresses to region names/identifier
 *-----------------------------------------------------------------------------
 */

typedef struct HN {
  long id;            /* hash code (address of function */
  char* name;         /* associated function name       */
  char* fname;        /*            file name           */
  int lno;            /*            line number         */
  uint32_t vtid;      /* associated region identifier   */
  struct HN* next;
} HashNode;

#define HASH_MAX 1021

static HashNode* htab[HASH_MAX];
static uint32_t n_htab_entries = 0;

/*
 * Stores function name `n' under hash code `h'
 */

static void hash_put(long h, const char* n, const char* fn, int lno) {
  long id = h % HASH_MAX;
  HashNode* add = (HashNode*)malloc(sizeof(HashNode));
  add->id = h;
  add->name  = (char*)n;
  add->fname = fn ? strdup(fn) : (char*)fn;
  add->lno   = lno;
  add->vtid = VT_NO_ID;
  add->next = htab[id];
  htab[id] = add;
  n_htab_entries++;
}

/*
 * Lookup hash code `h'
 * Returns hash table entry if already stored, otherwise NULL
 */

static HashNode* hash_get(long h) {
  long id = h % HASH_MAX;
  HashNode* curr = htab[id];
  while ( curr ) {
    if ( curr->id == h ) {
      return curr;
    }
    curr = curr->next;
  }
  return NULL;
}

/*
 * Get symbol table by 'nm'
 */

static void get_symtab(void)
{
  char* nm_cmd = NULL;
  char* nm_filename;
  FILE* nm_stream;

  char* line;
  size_t line_size;
  uint32_t lineno = 0;

  uint8_t parse_error = 0;

  VT_SUSPEND_MALLOC_TRACING(VT_CURRENT_THREAD);
  VT_SUSPEND_IO_TRACING(VT_CURRENT_THREAD);

  /* open nm-file, if given */
  nm_filename = vt_env_gnu_nmfile();
  if ( nm_filename != NULL && strlen(nm_filename) > 0 )
  {
    vt_cntl_msg(2, "Collecting symbols from file %s", nm_filename);

    /* open nm-file */
    if ( (nm_stream = fopen(nm_filename, "r")) == NULL )
      vt_error_msg("Could not open symbol list file %s", nm_filename);
  }
  /* otherwise, try to get symbol table automatically */
  else
  {
    char* apppath;
    char* nm;
    size_t nm_cmd_len;

    vt_cntl_msg(2, "Collecting symbols by 'nm'");

    /* get executable path specified by VT_APPPATH */
    apppath = vt_env_apppath();
    if ( apppath == NULL || strlen(apppath) == 0 )
    {
      vt_error_msg("Could not determine path of executable.\n"
                   "Please set the environment variable VT_APPPATH to the path "
                   "of your executable or set VT_GNU_NMFILE to a symbol list "
                   "file created with 'nm'.");
    }

    /* get nm command specified by VT_GNU_NM */
    nm = vt_env_gnu_nm();
    if ( nm == NULL )
    {
      vt_error_msg("VampirTrace was configured without an 'nm' command.\n"
                   "Please set the environment variable VT_GNU_NM to the 'nm' "
                   "command including command line switches which lists "
                   "symbol/addresses of an object file in BSD-style or set "
                   "VT_GNU_NMFILE to a pre-created symbol list file." );
    }

    /* allocate memory for nm command */
    nm_cmd_len = strlen(nm) + 1 + strlen(apppath) + 1;
    nm_cmd = (char*)malloc(nm_cmd_len * sizeof(char));
    if ( nm_cmd == NULL )
      vt_error();

    /* compose nm command */
    snprintf(nm_cmd, nm_cmd_len, "%s %s", nm, apppath);

    /* execute nm command */
    vt_cntl_msg(2, "Executing %s", nm_cmd);
    nm_stream = popen(nm_cmd, "r");
    /* error handling after pclose below */

    nm_filename = NULL;
  }

  /* allocate memory for lines */
  line = (char*)malloc(NM_LINE_BLK_LEN * sizeof(char));
  if ( line == NULL )
    vt_error();
  line_size = NM_LINE_BLK_LEN;

  /* read lines */

  while( nm_stream != NULL && fgets(line, line_size, nm_stream) )
  {
    char* col;
    char  delim[2] = " ";
    int   nc = 1;

    long  addr = 0;
    char* filename = NULL;
    char* funcname = NULL;
    unsigned int lno = VT_NO_LNO;

    lineno++;

    /* trigger a parse error, if line is empty */
    if ( strlen(line) == 0 )
    {
      parse_error = 1;
      break;
    }

    /* if line seems to be incomplete, enlarge line buffer and read the
       remaining line */
    while( !parse_error && line[strlen(line)-1] != '\n' )
    {
      char tmp[NM_LINE_BLK_LEN];

      /* read the remaining line; if it fails (EOF) the line seems to
         be complete after all */
      if ( !fgets(tmp, sizeof(tmp), nm_stream) )
        break;

      /* trigger a parse error, if line is to long (>NM_LINE_MAX_LEN) */
      if ( line_size + NM_LINE_BLK_LEN > NM_LINE_MAX_LEN )
      {
        parse_error = 1;
        break;
      }

      /* enlarge line buffer */
      line = (char*)realloc(line, (line_size + NM_LINE_BLK_LEN) * sizeof(char));
      if ( line == NULL )
        vt_error();
      line_size += NM_LINE_BLK_LEN;

      /* complete line */
      strcat(line, tmp);
    }
    if ( parse_error )
      break;

    /* chop new-line character from line */
    if ( line[strlen(line)-1] == '\n' )
      line[strlen(line)-1] = '\0';

    /* ignore line if it is empty */
    if ( *line == '\0' )
      continue;

    /* ignore nm input file name */
    if ( line[strlen(line)-1] == ':' )
      continue;

    /* split line to columns */
    col = strtok(line, delim);
    do
    {
      if ( nc == 1 ) /* column 1 (address) */
      {
        /* if there is no address in the first column the symbol could be
           defined within a shared object; try get its address later (nc==3) */
        if ( strlen(col) == 1 )
        {
          nc++; /* <- will be 3 in the next round */
          *delim = '\t';
        }
        /* otherwise, convert address string */
        else
        {
          addr = strtol(col, NULL, 16);
        }
      }
      else if ( nc == 2 ) /* column 2 (type) */
      {
        /* type must have a length of 1 */
        if ( strlen(col) != 1 )
        {
          parse_error = 1;
          break;
        }

        *delim = '\t';
      }
      else if ( nc == 3 ) /* column 3 (symbol) */
      {
        long soaddr;

        funcname = col;

        /* the symbol might be defined within a shared object; try to get
           its real address */
        if ( ( soaddr = GET_SO_FUNC_ADDR(funcname) ) != 0 )
          addr = soaddr;

        /* ignore function, if its address could not be determined */
        if ( addr == 0 )
          break;

        *delim = ':';
      }
      else if ( nc == 4 ) /* column 4 (filename) */
      {
        filename = col;
      }
      else /* column 5 (line) */
      {
        lno = atoi(col);
        if( lno == 0 ) lno = VT_NO_LNO;
        break;
      }

      nc++;
      col = strtok(0, delim);
    } while( col );

    /* stop reading file, if an parse error occurred */
    if ( parse_error )
    {
      break;
    }
    /* at least two columns must be read */
    else if ( nc < 3 )
    {
      parse_error = 1;
      break;
    }
    /* add symbol to hash table, if we have its address and name */
    else if ( addr > 0 && funcname )
    {
      char* n = strdup(funcname);
      char* p;

      if ( n == NULL )
        vt_error();

      /* chop function name at '??', if necessary */
      p = strstr(n, "??");
      if ( p != NULL && p != n )
        *p = '\0';

      hash_put(addr, n, filename, lno);
    }
  }

  /* close file/pipe stream */

  if ( nm_filename != NULL )
  {
    fclose(nm_stream);

    if ( parse_error )
    {
      vt_error_msg("%s:%u: could not be parsed.\n"
                   "Please check the content of %s for BSD-style.",
                   nm_filename, lineno, nm_filename);
    }
  }
  else
  {
    uint8_t nmcmd_error = (nm_stream == NULL || pclose(nm_stream) != 0);

    if ( parse_error )
    {
      vt_error_msg("Could not parse 'nm' output created with %s.\n"
                   "Please set the environment variable VT_GNU_NM to the 'nm' "
                   "command including command line switches which lists "
                   "symbol/addresses of an object file in BSD-style or set "
                   "VT_GNU_NMFILE to a pre-created symbol list file.",
                   nm_cmd);
    }
    else if ( nmcmd_error )
    {
      vt_error_msg("Failed to execute %s\n"
                   "Please set the environment variable VT_GNU_NM to the 'nm' "
                   "command including command line switches which lists "
                   "symbol/addresses of an object file in BSD-style or set "
                   "VT_GNU_NMFILE to a pre-created symbol list file.",
                   nm_cmd);
    }

    free(nm_cmd);
  }

  free(line);

  VT_RESUME_IO_TRACING(VT_CURRENT_THREAD);
  VT_RESUME_MALLOC_TRACING(VT_CURRENT_THREAD);
}

/*
 * Register new region
 */

static void register_region(uint32_t tid, HashNode* hn) {
  uint32_t fid = VT_NO_ID;
  uint32_t lno = VT_NO_LNO;

  /* -- register file if available -- */
  if (hn->fname != NULL)
  {
    fid = vt_def_scl_file(tid, hn->fname);
    lno = hn->lno;
  }

  /* -- register region and store region identifier -- */
  hn->vtid = vt_def_region(tid, hn->name, fid, lno, VT_NO_LNO,
                           NULL, VT_FUNCTION);
}

void gnu_finalize(void);
void __cyg_profile_func_enter(void* func, void* callsite);
void __cyg_profile_func_exit(void* func, void* callsite);

/*
 * Finalize instrumentation interface
 */

void gnu_finalize()
{
  int i, idx_min, idx_max;
  uint32_t min, max;
  double avg;
  min = 0xffffffff;
  max = 0;
  idx_min = idx_max = 0;
  avg = 0.0;

  for( i = 0; i < HASH_MAX; i++ )
  {
    uint32_t n_bucket_entries = 0;

    struct HN* p = htab[i];
    while( p )
    {
      n_bucket_entries++;

      /* Set assigned region id back to VT_NO_ID instead of freeing the
         hash-node, because after a fork the hash-node will be re-used for the
         child process. This implies a small/non-increasing memory leak. */
      p->vtid = VT_NO_ID;
      p = p->next;
    }
    if( n_bucket_entries < min ) {
      min = n_bucket_entries;
      idx_min = i;
    }
    if( n_bucket_entries > max ) {
      max = n_bucket_entries;
      idx_max = i;
    }
    vt_cntl_msg(3, "Hash bucket %i had %u entries (%.1f/1000)",
                i, n_bucket_entries, ((double)n_bucket_entries*1000)/n_htab_entries);
  }
  avg = (double)n_htab_entries / HASH_MAX;
  vt_cntl_msg( 3, "Hash statistics:\n"
                  "\tNumber of entries: %u\n"
                  "\tMin bucket size:   %u (%.1f/1000) at index %i\n"
                  "\tMax bucket size:   %u (%.1f/1000) at index %i\n"
                  "\tAvg bucket size:   %.1f",
                  n_htab_entries,
                  min, ((double)min*1000)/n_htab_entries, idx_min,
                  max, ((double)max*1000)/n_htab_entries, idx_max,
                  avg );
}

/*
 * This function is called at the entry of each function
 * The call is generated by the GNU/Intel (>=v10) compilers
 */

void __cyg_profile_func_enter(void* func, void* callsite) {
  long addr;
  uint32_t tid;
  uint64_t time;
  HashNode* hn;

  addr = GET_IA64_FUNC_ADDR(func);

  /* -- if not yet initialized, initialize VampirTrace -- */
  if ( gnu_init ) {
    gnu_init = 0;
    vt_open();
    vt_comp_finalize = gnu_finalize;
    get_symtab();
  }

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  /* -- get calling thread id -- */
  GET_THREAD_ID(tid);

  VT_SUSPEND_MALLOC_TRACING(tid);

  time = vt_pform_wtime();

  /* -- get region identifier -- */
  if ( (hn = hash_get(addr)) ) {
    if ( hn->vtid == VT_NO_ID ) {
      /* -- region entered the first time, register region -- */
#if (defined(VT_MT) || defined(VT_HYB))
      VTTHRD_LOCK_IDS();
      if( hn->vtid == VT_NO_ID )
        register_region(tid, hn);
      VTTHRD_UNLOCK_IDS();
#else /* VT_MT || VT_HYB */
      register_region(tid, hn);
#endif /* VT_MT || VT_HYB */
    }

    /* -- write enter record -- */
    vt_enter(tid, &time, hn->vtid);
  }

  VT_RESUME_MALLOC_TRACING(tid);
}

/*
 * This function is called at the exit of each function
 * The call is generated by the GNU/Intel (>=v10) compilers
 */

void __cyg_profile_func_exit(void* func, void* callsite) {
  long addr;
  uint32_t tid;
  uint64_t time;

  addr = GET_IA64_FUNC_ADDR(func);

  /* -- if VampirTrace already finalized, return -- */
  if ( !vt_is_alive ) return;

  /* -- get calling thread id -- */
  GET_THREAD_ID(tid);

  VT_SUSPEND_MALLOC_TRACING(tid);

  time = vt_pform_wtime();

  /* -- write exit record -- */
  if ( hash_get(addr) ) {
    vt_exit(tid, &time);
  }

  VT_RESUME_MALLOC_TRACING(tid);
}
