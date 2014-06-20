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

#include "vt_defs.h"
#include "vt_error.h"
#include "vt_trc.h"
#include "vt_mpifile.h"
#include "vt_thrd.h"

#include "util/hash.h"

#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>

/* 
 *-----------------------------------------------------------------------------
 *
 * MPI file management
 *
 *-----------------------------------------------------------------------------
 */

/* size of hash table (must be a power of two!) */
#define HASH_MAX 1024

typedef struct HN_file {
  const char*       fname;  /* file name */
  uint32_t          fid;    /* associated file identifier */
  struct HN_file*   next;
} HashNode_file;

static HashNode_file*   htab_mpifile[HASH_MAX];

static void hash_put( const char* n, uint32_t i )
{
  uint32_t id = vt_hash(n, strlen(n), 0) & (HASH_MAX - 1);

  HashNode_file* add = (HashNode_file*)malloc(sizeof(HashNode_file));
  add->fname = vt_strdup(n);
  add->fid = i;
  add->next = htab_mpifile[id];
  htab_mpifile[id] = add; 
}

static HashNode_file* hash_get( const char* n )
{
  uint32_t id = vt_hash(n, strlen(n), 0) & (HASH_MAX - 1);

  HashNode_file* curr = htab_mpifile[id];
  while ( curr ) {
    if ( strcmp( curr->fname, n ) == 0 ) {
      return curr;
    }
    curr = curr->next;
  }
  return NULL;
}

static int nfiles;
static int nmaxfiles;
static struct mpifh_fid_map {
  MPI_File mpifh;
  vt_mpifile_data file_data;
} *mpifh_fid_map = NULL;
static uint32_t mpifile_gid;

static int mpifile_initialized = 0;

/* Save the mapping fh-->id */
static vt_mpifile_data* store_id( const MPI_File fh, const uint32_t id )
{
  struct mpifh_fid_map *newentry;
  if( nfiles >= nmaxfiles ) 
    vt_error_msg( "Too many MPI_File handles" );

  /* nfiles is always the index to the next free entry */
  newentry = &(mpifh_fid_map[nfiles]);
  newentry->mpifh = fh;
  newentry->file_data.fid = id;
  newentry->file_data.handle = VTTHRD_IO_NEXT_HANDLE(VTTHRD_MY_VTTHRD);
  nfiles++;
  return &(newentry->file_data);
}

void vt_mpifile_init()
{
  if( !mpifile_initialized )
  {
    struct rlimit rl;

    mpifile_gid = vt_def_file_group( VT_CURRENT_THREAD, "MPI I/O" );
    if( getrlimit(RLIMIT_NOFILE, &rl) )
      vt_error_msg( "getrlimit() failed reading max no. of open files" );
    nmaxfiles = (rl.rlim_cur == RLIM_INFINITY) ? 131072 : (int)rl.rlim_cur;
    mpifh_fid_map  = (struct mpifh_fid_map*)calloc( nmaxfiles, sizeof(struct mpifh_fid_map) );
    if( !mpifh_fid_map )
      vt_error_msg( "Out of memory while allocating %i MPI_File handles", nmaxfiles );
    nfiles = 0;

    memset( htab_mpifile, 0, sizeof(htab_mpifile) );

    mpifile_initialized = 1;
  }
}

void vt_mpifile_finalize()
{
  int i;
  HashNode_file* tmp_file;

  for ( i = 0; i < HASH_MAX; i++ ) {
    while( htab_mpifile[i] )
    {
      tmp_file = htab_mpifile[i]->next;
      free( htab_mpifile[i] );
      htab_mpifile[i] = tmp_file;
    }
  }
  if( mpifh_fid_map ) {
    free( mpifh_fid_map );
    mpifh_fid_map = NULL;
  }
  nmaxfiles = 0;
  nfiles = 0;
  mpifile_initialized = 0;
}

/* Return complete data for the given fh */
vt_mpifile_data* vt_mpifile_get_data( const MPI_File fh )
{
  /* TODO: This should really be another container when many open files exist.
   * The list implies O(n) complexity for lookups!
   */
  int i = 0;
  struct mpifh_fid_map *m = mpifh_fid_map;

  while( (i < nfiles) && (fh != m->mpifh) )
    {
      i++;
      m++;
    }

  if( i < nfiles )
    return &(m->file_data);
  else {
    vt_error_msg("vt_mpifile_get_data: Cannot find file handle");
    return NULL;
  }
}

/* Return file id for the given filename
 * Create entry if necessary
 */
uint32_t vt_mpifilename_get_id( const char* fname )
{
  HashNode_file* entry;

  /* check if file id exists already */
  entry = hash_get( fname );
  if( entry )
    return entry->fid;
  else {
    uint32_t fid = vt_def_file( VT_CURRENT_THREAD, fname, mpifile_gid );
    hash_put( fname, fid );
    return fid;
  }
}

/* Release mapping for fh, return file id */
uint32_t vt_mpifile_free( const MPI_File fh )
{
  int i = 0;
  uint32_t fid;

  while( (i < nfiles) && (fh != mpifh_fid_map[i].mpifh) )
    i++;

  if( i < nfiles ) {
    fid = mpifh_fid_map[i].file_data.fid;
    nfiles--;
    if( i < nfiles )
      mpifh_fid_map[i] = mpifh_fid_map[nfiles];
    return fid;
  }
  else
    vt_error_msg("vt_mpifile_free: Cannot find file handle");

  return 0;
}

/* Create a definition entry for fname if necessary and a mapping from
 * the given fh to the file id
 * Returns: file id
 */
vt_mpifile_data* vt_mpifile_create( const MPI_File fh, const char* fname )
{
  uint32_t fid;
  HashNode_file* entry;

  /* check if file id exists already */
  entry = hash_get( fname );
  if( entry )
    fid = entry->fid;
  else {
    fid = vt_def_file( VT_CURRENT_THREAD, fname, mpifile_gid );
    hash_put( fname, fid );
  }

  /* save mapping fh-->fid */
  return store_id( fh, fid );
}
