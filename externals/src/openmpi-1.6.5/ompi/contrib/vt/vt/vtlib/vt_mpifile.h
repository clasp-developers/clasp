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

#ifndef _VT_MPIFILE_H
#define _VT_MPIFILE_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_inttypes.h"

#include "mpi.h"


/** Holds internal data associated with an MPI_File.
 */
typedef struct vt_mpifile_data {
  /** The fid is the id defined with OTF.
   */
  uint32_t fid;
  /** This one holds the matching id for MPI split collective routines.
   * MPI_File_read_all_{begin,end}
   * MPI_File_write_all_{begin,end}
   * MPI_File_read_at_all_{begin,end}
   * MPI_File_write_at_all_{begin,end}
   * MPI_File_read_ordered_{begin,end}
   * MPI_File_write_ordered_{begin,end}
   * According to the MPI standard, only one split collective may be active on a
   * given file handle at a time and the same thread must begin and end the
   * operation.
   */
  uint64_t split_collective_id;
  /** Handle id for differentiating accesses to the same file with different
   * file pointers.
   */
  uint64_t handle;
  /** Save also the datatype for evaluation within MPI_Get_count */
  MPI_Datatype datatype;
} vt_mpifile_data;

EXTERN void             vt_mpifile_init(void);
EXTERN void             vt_mpifile_finalize(void);

EXTERN vt_mpifile_data* vt_mpifile_get_data(const MPI_File fh);
EXTERN uint32_t         vt_mpifilename_get_id(const char* fname);
EXTERN vt_mpifile_data* vt_mpifile_store_id(const MPI_File fh, const uint32_t id);
EXTERN vt_mpifile_data* vt_mpifile_create(const MPI_File fh, const char *fname);
EXTERN uint32_t         vt_mpifile_free(const MPI_File fh);

#endif /* _VT_MPIFILE_H */
