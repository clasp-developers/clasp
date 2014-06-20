/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_IO_ROMIO_CONV_H
#define MCA_IO_ROMIO_CONV_H

/* Prefix that we add to all ROMIO symbols */
#ifdef ROMIO_PREFIX
#undef ROMIO_PREFIX
#endif
#define ROMIO_PREFIX(foo) mca_io_romio_dist_##foo

/* Section 9.2 */
/* Begin Prototypes */
#define MPI_File_open ROMIO_PREFIX(MPI_File_open)
#define MPI_File_close ROMIO_PREFIX(MPI_File_close)
#define MPI_File_delete ROMIO_PREFIX(MPI_File_delete)
#define MPI_File_set_size ROMIO_PREFIX(MPI_File_set_size)
#define MPI_File_preallocate ROMIO_PREFIX(MPI_File_preallocate)
#define MPI_File_get_size ROMIO_PREFIX(MPI_File_get_size)
#define MPI_File_get_group ROMIO_PREFIX(MPI_File_get_group)
#define MPI_File_get_amode ROMIO_PREFIX(MPI_File_get_amode)
#define MPI_File_set_info ROMIO_PREFIX(MPI_File_set_info)
#define MPI_File_get_info ROMIO_PREFIX(MPI_File_get_info)

/* Section 9.3 */
#define MPI_File_set_view ROMIO_PREFIX(MPI_File_set_view)
#define MPI_File_get_view ROMIO_PREFIX(MPI_File_get_view)

/* Section 9.4.2 */
#define MPI_File_read_at ROMIO_PREFIX(MPI_File_read_at)
#define MPI_File_read_at_all ROMIO_PREFIX(MPI_File_read_at_all)
#define MPI_File_write_at ROMIO_PREFIX(MPI_File_write_at)
#define MPI_File_write_at_all ROMIO_PREFIX(MPI_File_write_at_all)
#define MPI_File_iread_at ROMIO_PREFIX(MPI_File_iread_at)
#define MPI_File_iwrite_at ROMIO_PREFIX(MPI_File_iwrite_at)

/* Section 9.4.3 */
#define MPI_File_read ROMIO_PREFIX(MPI_File_read)
#define MPI_File_read_all ROMIO_PREFIX(MPI_File_read_all)
#define MPI_File_write ROMIO_PREFIX(MPI_File_write)
#define MPI_File_write_all ROMIO_PREFIX(MPI_File_write_all)

#define MPI_File_iread ROMIO_PREFIX(MPI_File_iread)
#define MPI_File_iwrite ROMIO_PREFIX(MPI_File_iwrite)

#define MPI_File_seek ROMIO_PREFIX(MPI_File_seek)
#define MPI_File_get_position ROMIO_PREFIX(MPI_File_get_position)
#define MPI_File_get_byte_offset ROMIO_PREFIX(MPI_File_get_byte_offset)

/* Section 9.4.4 */
#define MPI_File_read_shared ROMIO_PREFIX(MPI_File_read_shared)
#define MPI_File_write_shared ROMIO_PREFIX(MPI_File_write_shared)
#define MPI_File_iread_shared ROMIO_PREFIX(MPI_File_iread_shared)
#define MPI_File_iwrite_shared ROMIO_PREFIX(MPI_File_iwrite_shared)
#define MPI_File_read_ordered ROMIO_PREFIX(MPI_File_read_ordered)
#define MPI_File_write_ordered ROMIO_PREFIX(MPI_File_write_ordered)
#define MPI_File_seek_shared ROMIO_PREFIX(MPI_File_seek_shared)
#define MPI_File_get_position_shared ROMIO_PREFIX(MPI_File_get_position_shared)

/* Section 9.4.5 */
#define MPI_File_read_at_all_begin ROMIO_PREFIX(MPI_File_read_at_all_begin)
#define MPI_File_read_at_all_end ROMIO_PREFIX(MPI_File_read_at_all_end)
#define MPI_File_write_at_all_begin ROMIO_PREFIX(MPI_File_write_at_all_begin)
#define MPI_File_write_at_all_end ROMIO_PREFIX(MPI_File_write_at_all_end)
#define MPI_File_read_all_begin ROMIO_PREFIX(MPI_File_read_all_begin)
#define MPI_File_read_all_end ROMIO_PREFIX(MPI_File_read_all_end)
#define MPI_File_write_all_begin ROMIO_PREFIX(MPI_File_write_all_begin)
#define MPI_File_write_all_end ROMIO_PREFIX(MPI_File_write_all_end)
#define MPI_File_read_ordered_begin ROMIO_PREFIX(MPI_File_read_ordered_begin)
#define MPI_File_read_ordered_end ROMIO_PREFIX(MPI_File_read_ordered_end)
#define MPI_File_write_ordered_begin ROMIO_PREFIX(MPI_File_write_ordered_begin)
#define MPI_File_write_ordered_end ROMIO_PREFIX(MPI_File_write_ordered_end)

/* Section 9.5.1 */
#define MPI_File_get_type_extent ROMIO_PREFIX(MPI_File_get_type_extent)

/* Section 9.6.1 */
#define MPI_File_set_atomicity ROMIO_PREFIX(MPI_File_set_atomicity)
#define MPI_File_get_atomicity ROMIO_PREFIX(MPI_File_get_atomicity)
#define MPI_File_sync ROMIO_PREFIX(MPI_File_sync)

/* Section 4.13.3 */
#define MPI_File_set_errhandler ROMIO_PREFIX(MPI_File_set_errhandler)
#define MPI_File_get_errhandler ROMIO_PREFIX(MPI_File_get_errhandler)
/* End Prototypes */

#define MPI_Register_datarep ROMIO_PREFIX(MPI_Register_datarep)

/* JMS these don't seem to work... */
#define MPI_File_f2c ROMIO_PREFIX(MPI_File_f2c)
#define MPI_File_c2f ROMIO_PREFIX(MPI_File_c2f)

#define MPIO_Request_c2f ROMIO_PREFIX(MPIO_Request_c2f)
#define MPIO_Request_f2c ROMIO_PREFIX(MPIO_Request_f2c)

/* Conversion of MPI_File and MPIO_Request */
#define MPI_File ROMIO_PREFIX(MPI_File)

/* Open MPI's mpi.h #define's MPI_FILE_NULL, so we need to undef it
   here and allow it to be re-assigned to whatever ROMIO wants */
#undef MPI_FILE_NULL

/* Let's not use MPIR_Status_set_bytes */
#ifndef MPIR_Status_set_bytes
#define MPIR_Status_set_bytes ROMIO_PREFIX(MPIR_Status_set_bytes)
#endif

#endif /* MCA_IO_ROMIO_CONV_H */
